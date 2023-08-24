# get peaks for all states
# states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY")
states <- c("WA", "AZ")
usgs_fs_cl <- readRDS("data-raw/usgs_fs_comp4.RDS")
data.table::setDT(usgs_fs_cl)

for (x in seq_along(states)) {
  usgs_fs <- usgs_fs_cl[state == states[x] & !is.na(discharge) & minpeak < 1]
  data.table::setDT(usgs_fs)
  peaks_list <- vector("list", length = nrow(usgs_fs))

  # begin loop
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
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
  saveRDS(peaks_df, paste0("data-raw/peaks_df_", states[x], ".RDS"))
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

states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY", "WA", "AZ")

# which stations have no reported peaks?
p_ca <- readRDS("data-raw/peaks/peaks_df_CA.RDS")
p_co <- readRDS("data-raw/peaks/peaks_df_CO.RDS")
p_id <- readRDS("data-raw/peaks/peaks_df_ID.RDS")
p_mt <- readRDS("data-raw/peaks/peaks_df_MT.RDS")
p_nm <- readRDS("data-raw/peaks/peaks_df_NM.RDS")
p_nv <- readRDS("data-raw/peaks/peaks_df_NV.RDS")
p_or <- readRDS("data-raw/peaks/peaks_df_OR.RDS")
p_ut <- readRDS("data-raw/peaks/peaks_df_UT.RDS")
p_wy <- readRDS("data-raw/peaks/peaks_df_WY.RDS")
p_wa <- readRDS("data-raw/peaks/peaks_df_WA.RDS")
p_az <- readRDS("data-raw/peaks/peaks_df_AZ.RDS")
peaks <- list(p_ca, p_co, p_id, p_mt, p_nm, p_nv, p_or, p_ut, p_wy, p_wa, p_az)

miss_sites <- vector("list", length = length(states))
for (i in seq_along(states)) {
  temp <- usgs_fs_cl[state == states[i]]
  miss <- which(!(formatC(temp$site_no, width = 8, format = "d", flag = "0")
                  %in% unique(peaks[[i]]$id)))
  state <- rep(states[i], length(miss))
  missing <- temp$site_no[miss]
  df <- data.frame(state, missing)
  miss_sites[[i]] <- df
}
no_peaks_mat <- do.call(rbind, miss_sites)
no_peaks_df <- as.data.frame(no_peaks_mat)
saveRDS(no_peaks_df, "data-raw/no_peaks_df2.RDS")

# which stations do we not have data for?
rhv_ca <- readRDS("data-raw/rhv_tot/rhv_tot_CA.RDS")
rhv_co <- readRDS("data-raw/rhv_tot/rhv_tot_CO.RDS")
rhv_id <- readRDS("data-raw/rhv_tot/rhv_tot_ID.RDS")
rhv_mt <- readRDS("data-raw/rhv_tot/rhv_tot_MT.RDS")
rhv_nm <- readRDS("data-raw/rhv_tot/rhv_tot_NM.RDS")
rhv_nv <- readRDS("data-raw/rhv_tot/rhv_tot_NV.RDS")
rhv_or <- readRDS("data-raw/rhv_tot/rhv_tot_OR.RDS")
rhv_ut <- readRDS("data-raw/rhv_tot/rhv_tot_UT.RDS")
rhv_wy <- readRDS("data-raw/rhv_tot/rhv_tot_WY.RDS")
rhv_wa <- readRDS("data-raw/rhv_tot/rhv_tot_WA.RDS")
rhv_az <- readRDS("data-raw/rhv_tot/rhv_tot_AZ.RDS")

# rhv <- list(rhv_ca, rhv_co, rhv_id, rhv_mt, rhv_nm, rhv_nv, rhv_or, rhv_ut,
#             rhv_wy, rhv_wa, rhv_az)
states <- c("WA", "AZ")
rhv <- list(rhv_wa, rhv_az)

miss_sites <- vector("list", length = length(states))
for (i in seq_along(states)) {
  temp <- usgs_fs_cl[state == states[i]]
  miss <- which(!(formatC(temp$site_no, width = 8, format = "d", flag = "0")
                  %in% unique(rhv[[i]]$id)))
  state <- rep(states[i], length(miss))
  missing <- temp$site_no[miss]
  df <- data.frame(state, missing)
  miss_sites[[i]] <- df
}
no_peaks_mat <- do.call(rbind, miss_sites)
no_peaks_df <- as.data.frame(no_peaks_mat)
saveRDS(no_peaks_df, "data-raw/no_stationdata_df2.RDS")
