## libraries
# library(rsnodas2)
library(imputeTS)
library(lubridate)
library(data.table)
library(fasttime)
source("R/download_usgs_ind.R")
# from OG download_usgs function
library(anytime)
library(httr)
library(dplyr)
library(tidyr)

## retaining hourly data
hv <- function(usgs, timez) {
  # convert to data.table
  data.table::setDT(usgs)

  # enable grouping by individual time specs
  usgs[, year := substr(date, 1, 4)]
  usgs[, month := substr(date, 6, 7)]
  usgs[, day := substr(date, 9, 10)]
  usgs[, hour:= substr(time, 1, 2)]

  # calculate hourly maximum flow
  usgs[, max_flow := max(v00060), by = .(year, month, day, hour)]

  # convert from character to datetime object and set tz
  usgs[, hdt := fasttime::fastPOSIXct(paste0(date, " ", hour, ":00:00"),
                                      tz = "GMT"),
       by = .(year, month, day, hour)]
  usgs[, datetime := lubridate::force_tz(hdt, tzone = timez)]

  # save only hourly info
  grusgs <- usgs[, .(id = last(site_no),
                     max_flow = last(max_flow)),
                 by = datetime]
  grusgs
}

## clean data for specific stations
clean_dat <- function(site_no, timez) {
  usgs <- download_usgs(freq = "uv",
                        destpath = paste0(getwd(), "/data-raw/usgs"),
                        sites = as.character(site_no),
                        begin_date = as.Date("1980-01-02"),
                        end_date = Sys.Date()
  )

  # produce cleaned hourly data
  test <- hv(usgs, timez)

  # HOURLY create vector of dates/ids
  datetime <- seq(test$datetime[1],
                  lubridate::with_tz(Sys.Date(), tzone = timez),
                  by = 'hour'
  )
  id <- rep(usgs$site_no[1], times = length(datetime))
  df <- data.frame(id, datetime)

  # produce actual time series
  comp_test <- dplyr::left_join(df, test)
  imp_usgs <- imputeTS::na_interpolation(comp_test, option = "linear")

  imp_usgs
}

## get data frame with all stations
# import station ids
usgs_fs_cl <- data.table::fread("data-raw/usgs_fs_fin.csv")
missing_stations <- readRDS("data-raw/missing_stations_comp.RDS")
usgs_fs_miss <- usgs_fs_cl[site_no %in% missing_stations$missing]

# filter for states of interest - already have WY
states <- c("AZ", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY")

for (x in seq_along(states)) {
  usgs_abb <- usgs_fs_miss[state == states[x]]
  station_list <- vector("list", length = nrow(usgs_abb))
  vec <- c()
  for (i in seq_along(usgs_abb$site_no)) {
    station_list[[i]] <- tryCatch({clean_dat(usgs_abb$site_no[i],
                                             usgs_abb$tz[i])
    }, error = function(e) e)
    if (!inherits(station_list[[i]], "data.frame")) {
      vec[i] <- i
    }
  }

  if (!inherits(vec, "NULL")) {
    station_list2 <- station_list[-vec[which(!is.na(vec))]]
  } else {
    station_list2 <- station_list
  }

  state_mat <- do.call(rbind, station_list2)
  rhv_tot <- as.data.frame(state_mat)
  saveRDS(rhv_tot,
          file = paste0("data-raw/rhv_miss_", states[x], "2.RDS"),
          compress = TRUE)
}





## finding missing stations and peaks
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



# states


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










usgs <- download_usgs(freq = "uv",
                      destpath = paste0(getwd(), "/data-raw/usgs"),
                      sites = "09521100",
                      begin_date = as.Date("1980-01-02"),
                      end_date = Sys.Date()
)
head(usgs)


miss_ca <- readRDS("data-raw/rhv_miss/rhv_miss_CA.RDS")
miss_co <- readRDS("data-raw/rhv_miss/rhv_miss_CO.RDS")
miss_id <- readRDS("data-raw/rhv_miss/rhv_miss_ID.RDS")
miss_mt <- readRDS("data-raw/rhv_miss/rhv_miss_MT.RDS")
miss_nm <- readRDS("data-raw/rhv_miss/rhv_miss_NM.RDS")
miss_nv <- readRDS("data-raw/rhv_miss/rhv_miss_NV.RDS")
miss_or <- readRDS("data-raw/rhv_miss/rhv_miss_OR.RDS")
miss_ut <- readRDS("data-raw/rhv_miss/rhv_miss_UT.RDS")
miss_wy <- readRDS("data-raw/rhv_miss/rhv_miss_WY.RDS")
miss_wa <- readRDS("data-raw/rhv_miss/rhv_miss_WA.RDS")
miss_az <- readRDS("data-raw/rhv_miss/rhv_miss_AZ.RDS")

tot <- rbind(miss_ca, miss_co, miss_id, miss_mt, miss_nm, miss_nv, miss_or,
             miss_wy, miss_wa, miss_az)

length(unique(tot$id)) #125 stations reported








# how many stations were we actually able to get data for? ----------------
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
num <- 0
for (x in seq_along(states)) {
  # get all streamflow data
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)

  num <- num + length(unique(rhv_all$id))
}
