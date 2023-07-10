## peak detection function
peakdf <- function(df, fs, minpeak, min_cutoff, maj_cutoff = 1) {
  peakspor <- peakwindow(df$datetime, df$max_flow, minpeak = minpeak,
  )
  peakspordf <- peakspor[[1]]
  dt <- as.POSIXct(peakspor[[1]][, 3], "US/Pacific", origin = "1970-01-01")
  peakspordf$dt <- dt
  if (fs == TRUE) {
    peakspordf$type <- mm_cutoff(peakspordf, min_cutoff, maj_cutoff)
  } else {
    peakspordf$type <- f_cutoff(peakspordf, min_cutoff)
  }
  peakspordf$id <- rep(df$id[1], nrow(peakspordf))
  peakspordf[, c(8, 6, 5, 7)]
}

f_cutoff <- function(df, min_cutoff) {
  type <- vector()
  for (i in seq_len(nrow(df))){
    if (df$y[i] >= min_cutoff) {
      type[i] <- "flood"
    } else {
      type[i] <- "naf"
    }
  }
  type
}

mm_cutoff <- function(df, min_cutoff, maj_cutoff) {
  type <- vector()
  for (i in seq_len(nrow(df))){
    if (df$y[i] >= maj_cutoff) {
      type[i] <- "major"
    } else if (df$y[i] < maj_cutoff && df$y[i] >= min_cutoff) {
      type[i] <- "minor"
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
