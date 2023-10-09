peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS") # this file includes all
# peaks, even for initially missing stations (only 52 of them reported peaks)
data.table::setDT(peaks)

# add base_med column to peaks to fill in with baselines found using median
# of previous two weeks


# workflow for entire peaks dataframe
states <- c(#"NV",
  "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
for (x in seq_along(states)) {
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)

  peaks_sub <- peaks[state == states[x]]
  vec <- c()
  for (i in seq_len(nrow(peaks_sub))) {
    temp <- rhv_all[datetime %in% seq(peaks_sub$dt[i] - 1209600,
                                      peaks_sub$dt[i], by = "hour")
                    & id == peaks_sub$id[i]]
    vec[i] <- median(temp$max_flow)
  }
  peaks[state == states[x]]$base_med <- vec
}

saveRDS(peaks, "data-raw/peaks_fin/peaks_base_med.RDS")

# plot baselines for visuals
plot(temp$datetime, temp$max_flow)
abline(med, 0, col = "red")

### INITIAL ###
# just find baselines for WY
rhv_tot <- readRDS("data-raw/rhv_tot/rhv_tot_WY.RDS")
rhv_miss <- readRDS("data-raw/rhv_miss/rhv_miss_WY.RDS")

rhv_all <- rbind(rhv_tot, rhv_miss)
data.table::setDT(rhv_all)

# workflow for just the first peak in WY 235
peaks_sub <- peaks[state == "WY"]
peaks_base <- peaks_sub[, base := rep(0, times = nrow(peaks_sub))]
temp <- rhv_all[datetime %in% seq(peaks_base$dt[235] - 1209600,
                                  peaks_base$dt[235], by = "hour")
                & id == peaks_base$id[235]]
med <- median(temp$max_flow)

