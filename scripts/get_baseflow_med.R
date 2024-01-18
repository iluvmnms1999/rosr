library(data.table)
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS") # this file includes all
# peaks, even for initially missing stations (only 52 of them reported peaks)
data.table::setDT(peaks)

# add base_med column to peaks to fill in with baselines found using median
# of previous two weeks


# workflow for entire peaks dataframe
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
for (x in seq_along(states)) {
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)

  peaks_sub <- peaks[state == states[x]]
  vec <- c()
  for (i in seq_len(nrow(peaks_sub) - 1)) {
    if (peaks_sub$dt[i + 1] - peaks_sub$dt[i] < 1209600) {
      temp <- rhv_all[datetime %in% seq(peaks_sub$dt[i] - 1209600,
                                        peaks_sub$dt[i], by = "hour")
                      & id == peaks_sub$id[i]]
      vec[i] <- median(temp$max_flow)
    } else {
      temp <- rhv_all[datetime %in% seq(peaks_sub$dt[i] - 1209600,
                                        peaks_sub$dt[i], by = "hour")
                      & id == peaks_sub$id[i]]
      vec[i] <- median(temp$max_flow)
    }
  }
  peaks[state == states[x]]$base_med <- vec
}

saveRDS(peaks, "data-raw/peaks_fin/peaks_base_med.RDS")

# plot baselines for visuals
base <- readRDS("data-raw/peaks_fin/peaks_base_med.RDS")
base_NV <- base[state == "NV"][order(-y)]
rhv_tot <- readRDS("data-raw/rhv_tot/rhv_tot_NV.RDS")
rhv_miss <- readRDS("data-raw/rhv_miss/rhv_miss_NV.RDS")
rhv_all <- rbind(rhv_tot, rhv_miss)
data.table::setDT(rhv_all)

peaks_base <- base_NV[, base := rep(0, times = nrow(base_NV))]
temp <- rhv_all[datetime %in% seq(peaks_base$dt[1] - 1209600,
                                  peaks_base$dt[1], by = "hour")
                & id == peaks_base$id[1]]
med <- median(temp$max_flow)

pdf("figures/base_med_nv_ex.pdf", width = 6, height = 4)
ggplot(temp, aes(x = datetime, y = max_flow)) +
  geom_point() +
  geom_abline(slope = 0, intercept = med, col = "red")
dev.off()

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


# Re-calculation taking into account nearby peaks -------------------------
# add base_med col. to peaks df beforehand
peaks <- peaks[, base_med := 0]
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
for (x in seq_along(states)) {
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)

  peaks_sub <- peaks[state == states[x]]
  # using data.table for this step automatically converts to days but some are
  # wrong reported as days when they are actually hours
  peaks_diff <- peaks_sub |>
    group_by(id) |>
    mutate(time_diff = c(as.difftime(0, units = "secs"), diff(dt)))
  data.table::setDT(peaks_diff)
  vec <- c()
  for (i in seq_len(nrow(peaks_diff))) {
    if (peaks_diff$time_diff[i] < 1209600 & peaks_diff$time_diff[i] > 0) {
      temp <- rhv_all[datetime %in% seq(peaks_diff$dt[i - 1],
                                        peaks_diff$dt[i], by = "hour")
                      & id == peaks_diff$id[i]]
      vec[i] <- median(temp$max_flow)
    } else {
      temp <- rhv_all[datetime %in% seq(peaks_diff$dt[i] - 1209600,
                                        peaks_diff$dt[i], by = "hour")
                      & id == peaks_diff$id[i]]
      vec[i] <- median(temp$max_flow)
    }
  }
  peaks[state == states[x]]$base_med <- vec
}

saveRDS(peaks, "data-raw/peaks_fin/peaks_base_med_ref.RDS")

# CHECKS
new <- readRDS("data-raw/peaks_fin/peaks_base_med_ref.RDS")
og <- readRDS("data-raw/peaks_fin/peaks_base_med.RDS")
