## peak detection function
peakdf <- function(df, minpeak) {
  peakspor <- cardidates::peakwindow(df$datetime, df$max_flow, minpeak = minpeak
  )
  peakspordf <- peakspor[[1]]
  dt <- as.POSIXct(peakspor[[1]][, 3], "US/Pacific", origin = "1970-01-01")
  peakspordf$dt <- dt
  # peakspordf$type <- f_cutoff(peakspordf, cutoff) - add cutoff back in as
  # variable if you decide to specify type
  peakspordf$id <- rep(df$id[1], nrow(peakspordf))
  peakspordf[, c(7, 6, 5)]
}

# usgs_fs_cl <- readRDS("data-raw/usgs_fs/usgs_fs_comp4.RDS")
# usgs_fs <- usgs_fs_cl[state == "NV" & !is.na(discharge) & minpeak < 1]
# data.table::setDT(usgs_fs)
#
# rhv_tot <- readRDS("data-raw/rhv_tot/rhv_tot_NV.RDS")
# rhv_miss <- readRDS("data-raw/rhv_miss/rhv_miss_NV.RDS")
# rhv_fin <- rbind(rhv_tot, rhv_miss)
# data.table::setDT(rhv_fin)
#
# temp <- rhv_fin[id == formatC(usgs_fs$site_no[1],
#                               width = 8,
#                               format = "d",
#                               flag = "0")]
#
# peakdf(df = temp, minpeak = usgs_fs$minpeak[1])
