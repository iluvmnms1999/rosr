# get peaks for all states
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY", "WA", "AZ")
usgs_fs_cl <- readRDS("data-raw/usgs_fs_comp4.RDS")
data.table::setDT(usgs_fs_cl)

for (x in seq_along(states)) {
  usgs_fs <- usgs_fs_cl[state == states[x] & !is.na(discharge) & minpeak < 1]
  data.table::setDT(usgs_fs)
  peaks_list <- vector("list", length = nrow(usgs_fs))

  # begin loop
  rhv_tot <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  data.table::setDT(rhv_tot)
  vec <- c()
  for (i in seq_len(nrow(usgs_fs))) {
    temp <- rhv_tot[id == formatC(usgs_fs$site_no[i],
                                  width = 8,
                                  format = "d",
                                  flag = "0")]
    if (nrow(temp) == 0){
      peaks_list[[i]] <- NA
    } else {
      peaks_list[[i]] <- peakdf(df = temp, minpeak = usgs_fs$minpeak[i])
    }

    if (!inherits(peaks_list[[i]], "data.frame")) {
      vec[i] <- i
    }
  }

  if (!inherits(vec, "NULL")) {
    peaks_list2 <- peaks_list[-vec[which(!is.na(vec))]]
  } else {
    peaks_list2 <- peaks_list
  }

  peaks_mat <- do.call(rbind, peaks_list2)
  peaks_df <- as.data.frame(peaks_mat)
  saveRDS(peaks_df, paste0("data-raw/peaks_df_", states[x], "2.RDS"))
}

## peak detection function
peakdf <- function(df, minpeak) {
  peakspor <- cardidates::peakwindow(df$datetime, df$max_flow, minpeak = minpeak,
  )
  peakspordf <- peakspor[[1]]
  dt <- as.POSIXct(peakspor[[1]][, 3], "US/Pacific", origin = "1970-01-01")
  peakspordf$dt <- dt
  # peakspordf$type <- f_cutoff(peakspordf, cutoff) - add cutoff back in as
  # variable if you decide to specify type
  peakspordf$id <- rep(df$id[1], nrow(peakspordf))
  peakspordf[, c(7, 6, 5)]
}

# f_cutoff <- function(df, cutoff) {
#   type <- vector()
#   for (i in seq_len(nrow(df))){
#     if (df$y[i] >= cutoff) {
#       type[i] <- "flood"
#     } else {
#       type[i] <- "naf"
#     }
#   }
#   type
# }


