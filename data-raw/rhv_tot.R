## libraries
library(rsnodas2)
library(tidyverse)
library(lubridate)
library(imputeTS)
library(data.table)
library(lutz) # has tz_lookup_coords()
library(fasttime) # has fastPOSIXct()

# initial test with original function -> time = 15.96229 mins (BAD)
# need to get it below 30 secs
beg <- Sys.time()
clean_dat(09180000)
end <- Sys.time()
end - beg

flow <- download_usgs(
  freq = "uv",
  sites = "09180000",
  destpath = paste0(getwd(), "/data-raw/usgs"),
  begin_date = as.Date("1900-01-02"),
  end_date = Sys.Date()
)

setDT(flow)

## retaining hourly data
hv <- function(usgs) {
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
  usgs[, .(max_flow = max(v00060),
           hdt = fasttime::fastPOSIXct(paste0(date, " ", time), tz = "US/Pacific",
                            format = "%Y-%m-%d %H:%M")),
       by = .(year, month, day, hour)
       ]
  usgs[]
  # grusgs
}

## clean data for specific stations
clean_dat <- function(site_no) {
  usgs <- download_usgs(freq = "uv",
                        destpath = paste0(getwd(), "/data-raw/usgs"),
                        sites = as.character(site_no),
                        begin_date = as.Date("1900-01-02"),
                        end_date = Sys.Date()
  )

  # produce cleaned hourly data
  test <- hv(usgs)

  # HOURLY create vector of dates/ids
  datetime <- seq(test$datetime[1],
                  with_tz(Sys.Date(), tzone = "US/Pacific"),
                  by = 'hour'
  )
  id <- rep(usgs$site_no[1], times = length(datetime))
  df <- data.frame(id, datetime)

  # produce actual time series
  comp_test <- left_join(df, test)
  imp_usgs <- na_interpolation(comp_test, option = "linear")

  imp_usgs
}

## get data frame with all stations
stations <- c("10311000", "10310500", "10309000", "10311200",
              "10311100", "10308200")
beg_dates <- c("1939-05-12", "1948-03-01", "1900-01-02", "1976-07-01",
               "1976-06-01", "1960-09-01")
stat_list <- vector("list", length = length(stations))
for (i in seq_along(stations)) {
  stat_list[[i]] <- clean_dat(stations[i], beg_dates[i])
}
stat_mat <- do.call(rbind, stat_list)
rhv_tot <- as.data.frame(stat_mat)

usethis::use_data(rhv_tot, overwrite = TRUE)
