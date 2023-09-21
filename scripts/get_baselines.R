peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")
data.table::setDT(peaks)

# just find baselines for WY
rhv_tot <- readRDS("data-raw/rhv_tot/rhv_tot_WY.RDS")
rhv_miss <- readRDS("data-raw/rhv_miss/rhv_miss_WY.RDS")

rhv_all <- rbind(rhv_tot, rhv_miss)
data.table::setDT(rhv_all)

peaks_sub <- peaks[state == "WY"]

for (i in seq_len(nrow(peaks_sub))) {

}

seq(peaks_sub[1]$dt - 20, peaks_sub[1]$dt, by = "day")
