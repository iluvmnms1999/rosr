peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS") # this file includes all
# peaks, even for initially missing stations (only 52 of them reported peaks)
data.table::setDT(peaks)

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

plot(temp$datetime, temp$max_flow)
abline(med, 0, col = "red")

# workflow for entire peaks_tot dataframe
for (i in seq_len(nrow(peaks_sub))) {
  temp <- rhv_all[datetime %in% seq(peaks_base$dt[i] - 1209600,
                                         peaks_base$dt[i], by = "hour")
                  & id == peaks_base$id[i]]
  peaks_base$base <- median(temp$max_flow)
}

# 1209600 seconds in two weeks
seq(as.Date(peaks_sub[1]$dt - 1209600),
    as.Date(peaks_sub[1]$dt), by = "day")



