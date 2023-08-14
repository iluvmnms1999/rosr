df_peaks_all <- function(minpeaks_spec = 100) {
  peaks_list <- vector("list", length = nrow(cutoffs))
  for (i in seq_len(nrow(cutoffs))) {
    temp <- dplyr::filter(rhv_tot, id == cutoffs$stations[i])
    if (minpeaks_spec == 100) {
      peaks <- peakdf(temp, fs = cutoffs$fson[i],
                      minpeak = cutoffs$minpeaks100[i],
                      min_cutoff = cutoffs$minor[i],
                      maj_cutoff = cutoffs$major[i])
    } else if (minpeaks_spec == 75) {
      peaks <- peakdf(temp, fs = cutoffs$fson[i],
                      minpeak = cutoffs$minpeaks75[i],
                      min_cutoff = cutoffs$minor[i],
                      maj_cutoff = cutoffs$major[i])
    } else {
      peaks <- peakdf(temp, fs = cutoffs$fson[i],
                      minpeak = cutoffs$minpeaks50[i],
                      min_cutoff = cutoffs$minor[i],
                      maj_cutoff = cutoffs$major[i])
    }
    peaks_list[[i]] <- peaks
  }
  peaks_mat <- do.call(rbind, peaks_list)
  peaks_df <- as.data.frame(peaks_mat)
  peaks_df
}

## peak detection function
peakdf <- function(df, minpeak, cutoff) {
  peakspor <- cardidates::peakwindow(df$datetime, df$max_flow, minpeak = minpeak,
  )
  peakspordf <- peakspor[[1]]
  dt <- as.POSIXct(peakspor[[1]][, 3], "US/Pacific", origin = "1970-01-01")
  peakspordf$dt <- dt
  peakspordf$type <- f_cutoff(peakspordf, cutoff)
  peakspordf$id <- rep(df$id[1], nrow(peakspordf))
  peakspordf[, c(8, 6, 5, 7, 2)]
}

f_cutoff <- function(df, cutoff) {
  type <- vector()
  for (i in seq_len(nrow(df))){
    if (df$y[i] >= cutoff) {
      type[i] <- "flood"
    } else {
      type[i] <- "naf"
    }
  }
  type
}


## testing
# some important variables
stations <- c("10311000", "10310500", "10309000", "10311200",
              "10311100", "10308200", "10310000")
major <- c(8300, NA, 10000, NA, NA, 14354, 6600)
minor <- c(7000, 45, 5500, 26, 14.75, 7400, 1200)
fson <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
minpeaks <- c(0.2, 0.172, 0.41, 0.079, 0.117, 0.39, 0.43)

## get peaks for all stations with specified minpeaks according to stages
peaks_list <- vector("list", length = length(stations))
for (i in seq_along(stations)) {
  temp <- filter(shv_tot, id == stations[i])
  peaks <- peakdf(temp, fson[i], minpeak = minpeaks_50[i],
                  min_cutoff = minor[i], maj_cutoff = major[i])
  peaks_list[[i]] <- peaks
}
peaks_mat <- do.call(rbind, peaks_list50)
peaks_df <- as.data.frame(peaks_mat50)
