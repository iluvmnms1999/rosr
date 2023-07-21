## libraries
library(rsnodas2)
library(imputeTS)
library(lubridate)
library(data.table)
library(fasttime)
# library(lutz) - how I found time zones

# get tzs
# usgs_id[, tz := lutz::tz_lookup_coords(dec_lat_va, dec_long_va, method = "accurate")]

## retaining hourly data
hv <- function(usgs, timez) {
  # convert to data.table
  setDT(usgs)

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
usgs_fs_cl <- data.table::fread("data-raw/usgs_fs_cl.csv")
stat_list <- vector("list", length = nrow(usgs_fs_cl))
for (i in seq_along(usgs_fs_cl$site_no)) {
  stat_list[[i]] <- tryCatch({clean_dat(usgs_fs_cl$site_no[i],
                                        usgs_fs_cl$tz[i])
  }, error = function(e) e)
}
stat_mat <- do.call(rbind, stat_list)
rhv_tot <- as.data.frame(stat_mat)

usethis::use_data(rhv_tot, overwrite = TRUE)

beg <- Sys.time()
clean_dat(9429180, "America/Los_Angeles")
end <- Sys.time()
end - beg
