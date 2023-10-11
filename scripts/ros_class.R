#### classify ROS peaks
library(data.table)
library(dplyr)

#### import/prep necessary data ####
usgs_huc <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
snotel_huc <- readRDS("data-raw/snotel/snotel_huc.RDS")
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")

# add hucs to peaks
peaks <- peaks[, huc := rep(0, nrow(peaks))]
for (i in seq_along(usgs_huc$site_no)) {
  # add huc variable to peak data
  peaks[peaks$id == formatC(usgs_huc$site_no[i],
                            width = 8,
                            format = "d",
                            flag = "0")]$huc <- rep(usgs_huc$huc8[i],
                                                    times = nrow(peaks[peaks$id == formatC(usgs_huc$site_no[i],
                                                                                           width = 8,
                                                                                           format = "d",
                                                                                           flag = "0")]))
}

# subset peaks to just include those in the same hucs as snotel stations
peaks_match <- peaks[peaks$huc %in% as.numeric(snotel_huc$huc8)]

# add hucs and melt to snotel data for each station (TAKES FOREVER)
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/snotel_clean_", states[i], ".RDS"))
  # get rid of NA date rows
  snotel <- snotel[!is.na(date)]
  # add empty huc var
  snotel <- snotel[, huc := rep(0, nrow(snotel))]
  snotel <- snotel[, n_stat := rep(0, nrow(snotel))]

  # add huc variable
  station_num <- stringr::str_extract(unique(snotel$id), "(\\d+)")
  counts <- snotel_huc[state == states[i], length(unique(site_name)), by = huc8]
  for (x in seq_along(unique(snotel$id))) {
    # get vector of hucs for each unique snotel station in state
    huc <- snotel_huc[station_num[x] == stringr::str_extract(snotel_huc$site_name,
                                                             "(\\d+)")]$huc8

    # add huc variable to snotel measurement data
    snotel[stringr::str_extract(id, "(\\d+)") == station_num[x]]$huc <- rep(huc,
                                                                            times = nrow(snotel[stringr::str_extract(id, "(\\d+)") == station_num[x]]))
    snotel[stringr::str_extract(id, "(\\d+)") == station_num[x]]$n_stat <- rep(counts[counts$huc8 == huc]$V1,
                                                                               times = nrow(snotel[stringr::str_extract(id, "(\\d+)") == station_num[x]]))
  }
  snotel_melt <- snotel[, melt := swe + prec - shift(swe, type = "lead"),
                        by = id]

  # make negative melt values 0
  snotel_melt$melt[which(snotel_melt$melt < 0)] <- 0
  saveRDS(snotel_melt, paste0("data-raw/snotel/huc_melt/snotel_hucmelt_",
                              states[i], ".RDS"))
}

# add elevation (fp)
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/huc_melt/snotel_hucmelt_",
                           states[i], ".RDS"))
  snotel <- snotel[, elev := rep(0, nrow(snotel))]
  station_num <- stringr::str_extract(unique(snotel$id), "(\\d+)")
  for (x in seq_along(unique(snotel$id))) {
    elev <- snotel_huc[station_num[x] == stringr::str_extract(snotel_huc$site_name,
                                                              "(\\d+)")]$elev

    # add huc variable to snotel measurement data
    snotel[stringr::str_extract(id, "(\\d+)") == station_num[x]]$elev <- rep(elev,
                                                                             times = nrow(snotel[stringr::str_extract(id, "(\\d+)") == station_num[x]]))
  }
  saveRDS(snotel, paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_", states[i], ".RDS"))
}

#### Temp-based (P >= 10mm, SWE >= 10mm, T >= 1 [celsius]) ####

## import snotel data and add hucs to it
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/huc_melt/snotel_hucmelt_",
                           states[i], ".RDS"))

  ros_temp <- snotel[swe >= 10 & prec >= 10 & temp_degc >= 1]

  ros_days <- ros_temp %>% dplyr::group_by(date, huc) %>%
    dplyr::reframe(l_date = length(date), n_stat = n_stat) %>%
    filter(n_stat >= 2)
  intervals <- vector("list", length = length(ros_days$date))
  for (y in seq_along(ros_days$date)) {
    int <- seq(as.POSIXct(ros_days$date[y], tz = "US/Pacific"),
               by = "hour", length.out = 168)
    intervals[[y]] <- int
  }
  ros_days$date_ints <- intervals
  super_ros <- ros_days[ros_days$l_date / ros_days$n_stat >= 0.5, ]

  peaks_sub <- peaks_match[state == states[i] & peaks_match$huc %in% super_ros$huc]
  peaks_sub <- peaks_sub[, ros := rep(0, nrow(peaks_sub))]
  for (z in seq_along(peaks_sub$y)) {
    temp <- super_ros[super_ros$huc == peaks_sub$huc[z],]
    peaks_sub$ros[z] <- ifelse(as.POSIXct(peaks_sub$dt[z],
                                          format = "%Y-%m-%d",
                                          tz = "US/Pacific") %in%
                                 as.POSIXct(unlist(temp$date_ints),
                                            format = "%Y-%m-%d",
                                            tz = "US/Pacific",
                                            origin = "1970-01-01"),
                               "ros", "non-ros")
  }
  saveRDS(peaks_sub, paste0("data-raw/ros_class/huc_match/temp/ros_peaks_t_",
                            states[i], ".RDS"))
}

#### Split temp-based (P >= 10mm, SWE >= 10mm, T >= 2.6 | T >= 1.2 if elev <=2000 or >2000 respectively) ####
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                           states[i], ".RDS"))

  ros_temp_split <- snotel[swe >= 10 & prec >= 10 & fifelse(elev > 2000,
                                                           temp_degc >= 1.2,
                                                           temp_degc >= 2.6)]

  ros_days <- ros_temp_split %>% dplyr::group_by(date, huc) %>%
    dplyr::reframe(l_date = length(date), n_stat = n_stat) %>%
    filter(n_stat >= 2)
  intervals <- vector("list", length = length(ros_days$date))
  for (y in seq_along(ros_days$date)) {
    int <- seq(as.POSIXct(ros_days$date[y], tz = "US/Pacific"),
               by = "hour", length.out = 168)
    intervals[[y]] <- int
  }
  ros_days$date_ints <- intervals
  super_ros <- ros_days[ros_days$l_date / ros_days$n_stat >= 0.5, ]

  peaks_sub <- peaks_match[state == states[i] & peaks_match$huc %in% super_ros$huc]
  peaks_sub <- peaks_sub[, ros := rep(0, nrow(peaks_sub))]
  for (z in seq_along(peaks_sub$y)) {
    temp <- super_ros[super_ros$huc == peaks_sub$huc[z],]
    peaks_sub$ros[z] <- ifelse(as.POSIXct(peaks_sub$dt[z],
                                          format = "%Y-%m-%d",
                                          tz = "US/Pacific") %in%
                                 as.POSIXct(unlist(temp$date_ints),
                                            format = "%Y-%m-%d",
                                            tz = "US/Pacific",
                                            origin = "1970-01-01"),
                               "ros", "non-ros")
  }
  saveRDS(peaks_sub,
          paste0("data-raw/ros_class/huc_match/temp_split/ros_peaks_ts_",
                 states[i], ".RDS"))
}

#### PRISM: SWE-based (SWE >= 10mm, p >= 10mm, SWE/(P+SWE) >= 0.2) ####
#### SNOTEL: SWE-based ####
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/huc_melt/snotel_hucmelt_",
                           states[i], ".RDS"))

  ros_swe_snotel <- snotel[swe >= 10 & prec >= 10 & swe/(swe + prec) >= 0.2]

  # add info (variable) for how many of total snotels agree on ros days and only
  # retain hucs where there are more than two snotel stations
  ros_days <- ros_swe_snotel %>% dplyr::group_by(date, huc) %>%
    dplyr::reframe(l_date = length(date), n_stat = n_stat) %>%
    filter(n_stat >= 2)
  intervals <- vector("list", length = length(ros_days$date))
  for (y in seq_along(ros_days$date)) {
    int <- seq(as.POSIXct(ros_days$date[y], tz = "US/Pacific"),
               by = "hour", length.out = 168)
    intervals[[y]] <- int
  }
  ros_days$date_ints <- intervals
  super_ros <- ros_days[ros_days$l_date / ros_days$n_stat >= 0.5, ]

  peaks_sub <- peaks_match[state == states[i] & peaks_match$huc %in% super_ros$huc]
  peaks_sub <- peaks_sub[, ros := rep(0, nrow(peaks_sub))]
  for (z in seq_along(peaks_sub$y)) {
    temp <- super_ros[super_ros$huc == peaks_sub$huc[z],]
    peaks_sub$ros[z] <- ifelse(as.POSIXct(peaks_sub$dt[z],
                                          format = "%Y-%m-%d",
                                          tz = "US/Pacific") %in%
                                 as.POSIXct(unlist(temp$date_ints),
                                            format = "%Y-%m-%d",
                                            tz = "US/Pacific",
                                            origin = "1970-01-01"),
                               "ros", "non-ros")
  }
  saveRDS(peaks_sub, paste0("data-raw/ros_class/huc_match/swe_snotel/ros_peaks_ss_",
                            states[i], ".RDS"))
}

#### PRISM: Melt-based (SWE >= 10mm, p >= 10mm, melt/(P+melt) >= 0.2) ####
## get melt using prism prec
swe_prec <- read.csv("data-raw/ros_class/swe_prec.csv", header = TRUE)

# sort data frame by station
# shift prism values up one to match with snotel
swe_prec_ord <- swe_prec[order(swe_prec$id),]
swe_prec_ord2 <- swe_prec_ord %>%
  group_by(id) %>%
  mutate(prec_prism = lead(prec_prism))

swe_prec_diff2 <- swe_prec_ord2 %>%
  group_by(id) %>%
  mutate(melt = swe_snotel + prec_prism - lead(swe_snotel))

# make negative melt values 0
swe_prec_diff2$melt[which(swe_prec_diff2$melt < 0)] <- 0
swe_prec_diff2 <- ungroup(swe_prec_diff2)

#### SNOTEL: Melt-based ####
## get melt using snotel prec
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/huc_melt/snotel_hucmelt_",
                           states[i], ".RDS"))

  ros_melt_snotel <- snotel[swe >= 10 & prec >= 10 & melt/(melt + prec) >= 0.2]

  # add info (variable) for how many of total snotels agree on ros days and only
  # retain hucs where there are more than two snotel stations
  ros_days <- ros_melt_snotel %>% dplyr::group_by(date, huc) %>%
    dplyr::reframe(l_date = length(date), n_stat = n_stat) %>%
    filter(n_stat >= 2)
  intervals <- vector("list", length = length(ros_days$date))
  for (y in seq_along(ros_days$date)) {
    int <- seq(as.POSIXct(ros_days$date[y], tz = "US/Pacific"),
               by = "hour", length.out = 168)
    intervals[[y]] <- int
  }
  ros_days$date_ints <- intervals
  super_ros <- ros_days[ros_days$l_date / ros_days$n_stat >= 0.5, ]

  peaks_sub <- peaks_match[state == states[i] & peaks_match$huc %in% super_ros$huc]
  peaks_sub <- peaks_sub[, ros := rep(0, nrow(peaks_sub))]
  for (z in seq_along(peaks_sub$y)) {
    temp <- super_ros[super_ros$huc == peaks_sub$huc[z],]
    peaks_sub$ros[z] <- ifelse(as.POSIXct(peaks_sub$dt[z],
                                          format = "%Y-%m-%d",
                                          tz = "US/Pacific") %in%
                                 as.POSIXct(unlist(temp$date_ints),
                                            format = "%Y-%m-%d",
                                            tz = "US/Pacific",
                                            origin = "1970-01-01"),
                               "ros", "non-ros")
  }
  saveRDS(peaks_sub, paste0("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_",
                            states[i], ".RDS"))
}


#### plots ####
rhv_og <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_CA.RDS"))
rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_CA.RDS"))
rhv_tot <- rbind(rhv_og, rhv_miss)

sub <- dplyr::filter(rhv_tot, id == as.character(id) &
                       dplyr::between(datetime,
                                      lubridate::ymd_h(beg_date,
                                                       tz = "US/Pacific"),
                                      lubridate::ymd_h(end_date,
                                                       tz = "US/Pacific")
                       )
)

# add points for defined peaks
peaks_df <- df_peaks_filt(station, peaks)

# set maximum of y axis
y_max <- max(dplyr::filter(peaks_df,
                           dplyr::between(dt,
                                          lubridate::ymd_h(beg_date,
                                                           tz = "US/Pacific"),
                                          lubridate::ymd_h(end_date,
                                                           tz = "US/Pacific")
                           )
)$y
)

# create plot of data and peak classification
ggplot2::ggplot(sub, ggplot2::aes(x = as.POSIXct(datetime),
                                  y = max_flow)) +
  geom_point(size = .2, col = "grey50") +
  ggtitle(paste0("Streamflow Peaks at Station ",
                 as.character(station))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("DateTime") +
  scale_x_datetime(
    limits = c(lubridate::ymd_h(beg_date, tz = "US/Pacific"),
               lubridate::ymd_h(end_date, tz = "US/Pacific")),
    date_breaks = "1 week",
    date_labels = "%m-%d-%y"
  ) +
  ylab("Discharge amount (ft^3 per sec)") +
  scale_y_continuous(
    limits = c(0, ifelse(y_max + 0.2 * y_max > 20000,
                         plyr::round_any(y_max + 0.2 * y_max, 10000),
                         ifelse(y_max + 0.2 * y_max > 2000,
                                plyr::round_any(y_max + 0.2 * y_max, 1000),
                                ifelse(y_max + 0.2 * y_max > 200,
                                       plyr::round_any(y_max + 0.2 * y_max,
                                                       100),
                                       plyr::round_any(y_max + 0.2 * y_max,
                                                       10))))),
    n.breaks = 5, breaks = waiver()) +
  geom_point(data = peaks_df, aes(as.POSIXct(dt), y),
             color = ifelse(peaks_df$type %in% c("major", "flood"),
                            "red3",
                            ifelse(peaks_df$type == "minor",
                                   "gold", "green3")
             )
  )


#### NOTES ####
library(rsnodas2)
library(tidyverse)
library(sf)
library(ggplot2)
library(gridExtra)
library(lubridate)

# huc 10 findings:
# usgs's 10308200 and 10309000 match with snotel 633
# usgs 10310000 matches with snotels 1049 and 1050

# load peaks
peaks_df100 <- read.csv("data/rosclass/peaks_df100.csv", header = TRUE)
peaks_df75 <- read.csv("data/rosclass/peaks_df75.csv", header = TRUE)
peaks_df50 <- read.csv("data/rosclass/peaks_df50.csv", header = TRUE)


# df for snotel stations in huc 8 (16050201)
# "1051:CA:SNTL", "1049:CA:SNTL", "1050:CA:SNTL", "633:CA:SNTL", "778:CA:SNTL",
# "697:CA:SNTL", "462:CA:SNTL"

# download snotel data
snotel_ddf <- download_snotel(freq = "daily",
                              destpath = "data/snotel/snotel_ddf",
                              sites = c("1051:CA:SNTL", "1049:CA:SNTL",
                                        "1050:CA:SNTL", "633:CA:SNTL", "778:CA:SNTL",
                                        "697:CA:SNTL", "462:CA:SNTL"),
                              begin_date = as.Date("1991-10-01"),
                              end_date = Sys.Date()
)

snotel_melt <- snotel_ddf %>%
  select(id, date,
         precipitation_increment_mm,
         snow_water_equivalent_mm_start_of_day_values) %>%
  dplyr::rename(precip_snotel = precipitation_increment_mm,
                swe_snotel = snow_water_equivalent_mm_start_of_day_values) %>%
  mutate(sn_melt = swe_snotel + precip_snotel - lead(swe_snotel))

# replace id with numeric in snotel_ddf and order by id
snotel_melt$id <- as.numeric(substr(formatC(snotel_melt$id,
                                            width = 4,
                                            format = "d",
                                            flag = "0"
), 1, 4
)
)

snotel_melt_ord <- snotel_melt[order(snotel_melt$id),]

usgs_ddf <- download_usgs(freq = "dv",
                          destpath = paste0(getwd(), "/usgs"),
                          sites = c("10311000", "10310500",
                                    "10309000", "10311200", "10311100",
                                    "10308200", "10310000"),
                          begin_date = as.Date("1900-01-01"),
                          end_date = Sys.Date()
)

## find dates (from daily SNOTEL data) with correct ROS conditions
# snotel elevations by station (https://www.nrcs.usda.gov/wps/portal/wcc/home/quicklinks/imap#version=167&elements=&networks=SNTL&states=CA&counties=!&hucs=&minElevation=&maxElevation=&elementSelectType=any&activeOnly=true&activeForecastPointsOnly=false&hucLabels=false&hucIdLabels=false&hucParameterLabels=false&stationLabels=id&overlays=&hucOverlays=2&basinOpacity=75&basinNoDataOpacity=25&basemapOpacity=100&maskOpacity=0&mode=stations&openSections=,dataElement,parameter,date,basin,options,elements,location,networks,overlays,labels,&controlsOpen=true&popup=697:CA:SNTL&popupMulti=&popupBasin=&base=esriNgwm&displayType=inventory&basinType=6&dataElement=WTEQ&depth=-8&parameter=PCTMED&frequency=DAILY&duration=I&customDuration=&dayPart=E&year=2023&month=6&day=18&monthPart=E&forecastPubMonth=6&forecastPubDay=1&forecastExceedance=50&useMixedPast=true&seqColor=1&divColor=7&scaleType=D&scaleMin=&scaleMax=&referencePeriodType=POR&referenceBegin=1991&referenceEnd=2020&minimumYears=20&hucAssociations=true&lat=38.7576&lon=-119.6387&zoom=9.5)
# tabulated elevation values (https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&state=&counttype=statelist)
#   1050: 8,557
#   1051: 8,129
#   462: 8,661
#   778: 6,063
#   1049: 8,017
#   633: 8,306
#   697: 7,736

ros_days <- read.csv("data/snotel_ros_days.csv", header = TRUE)

# TEMP-BASED
# add elevation to snotel_ddf


# ros_days2 <- snotel_ddf %>%
#   filter(snow_water_equivalent_mm_start_of_day_values >= 10 &
#            precipitation_increment_mm >= 10 &
#            snow_water_equivalent_mm_start_of_day_values /
#            (snow_water_equivalent_mm_start_of_day_values +
#               precipitation_increment_mm) >= .2) %>%
#   select(id, date,
#          precipitation_increment_mm,
#          snow_water_equivalent_mm_start_of_day_values) %>%
#   rename(precip = precipitation_increment_mm,
#          swe = snow_water_equivalent_mm_start_of_day_values)

# corrected method incorporating melt (prism precip)
ros_days2 <- swe_prec_diff %>%
  filter(swe_snotel >= 10 &
           prec_prism >= 10 &
           melt / (melt + prec_prism) >= 0.2)

# corrected method incorporating melt (snotel precip)
ros_days3 <- snotel_melt %>%
  filter(swe_snotel >= 10 &
           precip_snotel >= 10 &
           sn_melt / (sn_melt + precip_snotel) >= 0.2)

# ros_floods <- usgs_ddf %>%
#   filter(as.Date(datetime) %in% ros_days$date)
#
# ros_peakdays1 <- ros_days[as.POSIXct(ros_days$date, tz = "US/Pacific")
#                          %in% as.POSIXct(peaks_df100$dt, "%Y-%m-%d",
#                                          tz = "US/Pacific")]
#
# ros_peakdays2 <- ros_days[as.POSIXct(ros_days$date, tz = "US/Pacific")
#                          %in% as.POSIXct(peaks_df100$dt, "%Y-%m-%d",
#                                          tz = "US/Pacific")]


# figure out which peaks happened on ROS days -- need to expand windows (there
# may be a slight delay between ROS event and flood)

# create windows around ros events
intervals <- vector("list", length = length(ros_days$date))
for (i in seq_along(ros_days$date)) {
  int <- seq(as.POSIXct(ros_days$date[i], tz = "US/Pacific"),
             by = "day", length.out = 5)
  intervals[[i]] <- int
}
ros_days$date_ints <- intervals

# classify ros vs non-ros peaks v1
peaks_df100v1 <- peaks_df100
peaks_df100v1$ros <- ifelse(as.POSIXct(peaks_df100v1$dt,
                                       "%Y-%m-%d", tz = "US/Pacific") %in%
                              unlist(ros_days$date_ints), "ros", "non-ros")

# classify ros vs non-ros peaks v2 (temp method)
dup_ros_days2 <- ros_days1 %>% dplyr::group_by(date) %>% dplyr::summarise(length(date))
intervalsv2 <- vector("list", length = length(dup_ros_days2$date))
for (i in seq_along(dup_ros_days2$date)) {
  int <- seq(as.POSIXct(dup_ros_days2$date[i], tz = "US/Pacific"),
             by = "day", length.out = 7)
  intervalsv2[[i]] <- int
}
dup_ros_days2$date_ints <- intervalsv2
super_ros2 <- dup_ros_days2[dup_ros_days2$`length(date)` / 7 > 0.5, ]

peaks_df100v2 <- peaks_df100
peaks_df100v2$ros <- ifelse(as.POSIXct(peaks_df100v2$dt,
                                       format = "%Y-%m-%d", tz = "US/Pacific") %in%
                              as.POSIXct(unlist(super_ros2$date_ints), format = "%Y-%m-%d",
                                         tz = "US/Pacific", origin = "1970-01-01"),
                            "ros", "non-ros")

# classify 1997 ros floods as ros even though data is missing
for (i in seq_along(peaks_df100v2$dt)) {
  if(as.POSIXct(peaks_df100v2$dt[i], tz = "US/Pacific") %in%
     seq(ymd_hms(19970101000000, tz = "US/Pacific"),
         ymd_hms(19970106000000, tz = "US/Pacific"),
         by = "hour")) {
    peaks_df100v2$ros[i] <- "ros"
  }
}

# classify ros vs non-ros peaks v3 (melt method - prism)
dup_ros_days3 <- ros_days2 %>% dplyr::group_by(date) %>% dplyr::summarise(length(date))
intervalsv3 <- vector("list", length = length(dup_ros_days3$date))
for (i in seq_along(dup_ros_days3$date)) {
  int <- seq(as.POSIXct(dup_ros_days3$date[i], tz = "US/Pacific"),
             by = "day", length.out = 7)
  intervalsv3[[i]] <- int
}
dup_ros_days3$date_ints <- intervalsv3
super_ros3 <- dup_ros_days3[dup_ros_days3$`length(date)` / 7 > 0.5, ]


peaks_df100v5 <- peaks_df100
peaks_df100v5$ros <- ifelse(as.POSIXct(peaks_df100v3$dt,
                                       format = "%Y-%m-%d", tz = "US/Pacific") %in%
                              as.POSIXct(unlist(super_ros3$date_ints), format = "%Y-%m-%d",
                                         tz = "US/Pacific", origin = "1970-01-01"),
                            "ros", "non-ros")

# classify 1997 ros floods as ros even though data is missing
for (i in seq_along(peaks_df100v5$dt)) {
  if(as.POSIXct(peaks_df100v5$dt[i], tz = "US/Pacific") %in%
     seq(ymd_hms(19970101000000, tz = "US/Pacific"),
         ymd_hms(19970106000000, tz = "US/Pacific"),
         by = "hour")) {
    peaks_df100v5$ros[i] <- "ros"
  }
}



# classify 1997 ros floods as ros even though data is missing
for (i in seq_along(peaks_df100v4$dt)) {
  if(as.POSIXct(peaks_df100v4$dt[i], tz = "US/Pacific") %in%
     seq(ymd_hms(19970101000000, tz = "US/Pacific"),
         ymd_hms(19970106000000, tz = "US/Pacific"),
         by = "hour")) {
    peaks_df100v4$ros[i] <- "ros"
  }
}













