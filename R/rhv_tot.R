## libraries
library(rsnodas2)
# library(tidyverse)
# library(lubridate)
library(imputeTS)
library(data.table)
library(lutz) # has tz_lookup_coords()
library(fasttime) # has fastPOSIXct() -- having to convert everythi

# initial test with original function -> time = 15.96229 mins (BAD)
# need to get it below 30 secs
# GOT IT DOWN TO 1.621509 mins FOR ONE STATION
beg <- Sys.time()
clean_dat(09180000)
end <- Sys.time()
end - beg

# tz testing
# usgs_id[, tz := tz_lookup_coords(dec_lat_va, dec_long_va, method = "accurate")]
usgs_id <- fread("data-raw/usgs_id.csv")

# test station
flow <- download_usgs(
  freq = "uv",
  sites = "09180000",
  destpath = paste0(getwd(), "/data-raw/usgs"),
  begin_date = as.Date("1900-01-02"),
  end_date = Sys.Date()
)

setDT(flow)

## retaining hourly data
hv <- function(usgs, timez) {
  setDT(usgs)
  # usgs$year <- year(usgs$date)
  usgs[, year := substr(date, 1, 4)]
  # usgs$month <- formatC(month(usgs$date), width = 2, format = "d", flag = "0")
  usgs[, month := substr(date, 6, 7)]
  # usgs$day <- formatC(mday(usgs$date), width = 2, format = "d", flag = "0")
  usgs[, day := substr(date, 9, 10)]
  # usgs$hour <- substr(usgs$time, start = 1, stop = 2)
  usgs[, hour:= substr(time, 1, 2)]
  # grusgs <- usgs %>%
  # group_by(year, month, day, hour) %>%
  # mutate(max_flow = max(v00060)) %>%
  # mutate(hdt = ymd_h(as.integer(paste0(year, month, day, hour)),
  #                    tz = "US/Pacific")) %>%
  # ungroup() %>%
  # group_by(hdt) %>%
  # summarise(
  #   id = last(site_no),
  #   max_flow = last(max_flow),
  #   datetime = last(hdt)
  # ) %>%
  # select(-hdt)
  usgs[, max_flow := max(v00060), by = .(year, month, day, hour)]
  usgs[, hdt := fasttime::fastPOSIXct(paste0(date, " ", hour, ":00:00"),
                                      tz = "GMT"),
       by = .(year, month, day, hour)]
  usgs[, datetime := lubridate::force_tz(hdt, tzone = timez)]
  grusgs <- usgs[, .(id = last(site_no),
           max_flow = last(max_flow)),
       by = datetime]
  # grusgs
  grusgs
}

## clean data for specific stations
clean_dat <- function(site_no, timez) {
  usgs <- download_usgs(freq = "uv",
                        destpath = paste0(getwd(), "/data-raw/usgs"),
                        sites = as.character(site_no),
                        begin_date = as.Date("1900-01-02"),
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
beg <- Sys.time()
stations <- c(10311000, 10309010, 10310500, 10309000, 10311200, 10308200,
              10310000, 10311100)
# beg_dates <- c("1939-05-12", "1948-03-01", "1900-01-02", "1976-07-01",
#                "1976-06-01", "1960-09-01")
timezones <- rep("America/Los_Angeles", times = length(stations))
stat_list <- vector("list", length = length(stations))
for (i in seq_along(stations)) {
  stat_list[[i]] <- clean_dat(stations[i], timezones[i])
}
stat_mat <- do.call(rbind, stat_list)
rhv_tot <- as.data.frame(stat_mat)
end <- Sys.time()
end - beg

usethis::use_data(rhv_tot, overwrite = TRUE)

write.csv(rhv_tot, "data-raw/shv_tot_UPD071323.csv", row.names = FALSE)







## TEST
beg <- Sys.time()
x <- clean_dat(09180000, "America/Denver")
end <- Sys.time()
end - beg
