# libraries
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
id <- c("1051:CA:SNTL", "1049:CA:SNTL", "1050:CA:SNTL", "633:CA:SNTL",
        "778:CA:SNTL", "697:CA:SNTL", "462:CA:SNTL")
elev <- c(8129, 8017, 8557, 8306, 6063, 7736, 8661)
vec <- vector("list", length = length(id))
for (i in seq_len(length(id))) {
  vec[[i]] <- rep(elev[i], times = sum(snotel_ddf$id == id[i]))
}
snotel_ddf$elevation <- unlist(vec)

ros_days1 <- snotel_ddf %>%
  filter(snow_water_equivalent_mm_start_of_day_values >= 10 &
           ifelse(elevation > 2000,
                  air_temperature_observed_degc_start_of_day_values >= 1.2,
                  air_temperature_observed_degc_start_of_day_values >= 2.6) &
           precipitation_increment_mm >= 10) %>% 
  select(id, date, air_temperature_observed_degc_start_of_day_values,
         precipitation_increment_mm,
         snow_water_equivalent_mm_start_of_day_values) %>% 
  dplyr::rename(temp = air_temperature_observed_degc_start_of_day_values,
                precip = precipitation_increment_mm,
                swe = snow_water_equivalent_mm_start_of_day_values)

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

# classify ros vs non-ros peaks v4 (melt method - snotel)
dup_ros_days4 <- ros_days3 %>% dplyr::group_by(date) %>% dplyr::summarise(length(date))
intervalsv4 <- vector("list", length = length(dup_ros_days4$date))
for (i in seq_along(dup_ros_days4$date)) {
  int <- seq(as.POSIXct(dup_ros_days4$date[i], tz = "US/Pacific"),
             by = "day", length.out = 7)
  intervalsv4[[i]] <- int
}
dup_ros_days4$date_ints <- intervalsv4
super_ros4 <- dup_ros_days4[dup_ros_days4$`length(date)` / 7 > 0.5, ]


peaks_df100v4 <- peaks_df100
peaks_df100v4$ros <- ifelse(as.POSIXct(peaks_df100v4$dt, 
                                       format = "%Y-%m-%d", tz = "US/Pacific") %in% 
                              as.POSIXct(unlist(super_ros4$date_ints), format = "%Y-%m-%d",
                                         tz = "US/Pacific", origin = "1970-01-01"),
                            "ros", "non-ros")

# classify 1997 ros floods as ros even though data is missing
for (i in seq_along(peaks_df100v4$dt)) {
  if(as.POSIXct(peaks_df100v4$dt[i], tz = "US/Pacific") %in%
     seq(ymd_hms(19970101000000, tz = "US/Pacific"),
         ymd_hms(19970106000000, tz = "US/Pacific"),
         by = "hour")) {
    peaks_df100v4$ros[i] <- "ros"
  }
}


# what's going on during peak times at snotels?
filter(ros_days, year(date) == 2017 & id %in%
         c("1050:CA:SNTL", "1049:CA:SNTL"))
filter(peaks_df100, year(dt) == 1997 & id == 10310000)

filter(ros_days, year(date) == 1996 & id == "633:CA:SNTL")
filter(peaks_df100, year(dt) == 1997 & id %in% c(10308200, 10309000))

# summary of three classified ros groups
summary(filter(peaks_df100v2, ros == "ros")$y)
summary(filter(peaks_df100v2, ros == "pot-ros")$y)
summary(filter(peaks_df100v2, ros == "non-ros")$y)

# graphs
# max flow for all floods
ggplot(peaks_df100v2, aes(x = y)) +
  geom_density()
# just ros
ggplot(filter(peaks_df100v2, ros == "ros" &
                id %in% c(10311000, 10309000, 10308200, 10310000)),
       aes(x = y)) +
  geom_density()
# just potential ros
ggplot(filter(peaks_df100v2, ros == "pot-ros" &
                id %in% c(10311000, 10309000, 10308200, 10310000)),
       aes(x = y)) +
  geom_density()
# just non-ros
ggplot(filter(peaks_df100v2, ros == "non-ros" &
                id %in% c(10311000, 10309000, 10308200, 10310000)),
       aes(x = y)) +
  geom_density()

# densities before log transform
peaks_df100v2 %>%
  ggplot(aes(x = y, color = ros)) +
  geom_density() +
  #scale_x_continuous(breaks = seq(0, 11, 1), limits = c(0, 11)) +
  #scale_y_continuous(breaks = seq(0, 0.3, 0.05), limits = c(0, 0.3)) +
  xlab("Streamflow (ft ^ 3 per sec)") +
  ylab("Density") +
  ggtitle("ROS Classification of Streamflow Distributions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#bf812d", "#41ab5d", "#335978"),
                     labels = c("Non-ROS", "Potential ROS", "ROS")) +
  labs(color = "ROS Class")

# log density plot
png("figures/srs/rosdistr.png", width = 5, height = 7, units = "in", res = 700)
g1 <- peaks_df100v2 %>%
  ggplot(aes(x = log(y), color = ros)) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 12, 1), limits = c(0, 12)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05), limits = c(0, 0.3)) +
  xlab("Log streamflow (log(ft ^ 3 per sec))") +
  ylab("Density") +
  ggtitle("ROS Classification of Streamflow Distributions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#bf812d", "#41ab5d", "#335978"),
                     labels = c("Non-ROS", "Potential ROS", "ROS")) +
  labs(color = "ROS Class")

g2 <- ggplot(sub, aes(x = as.POSIXct(datetime), y = max_flow)) +
  geom_point(size = .2, col = "grey50") +
  ggtitle("2017: Streamflow Peaks at Station 10311100") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("DateTime") +
  xlim(ymd_h(2017020101, tz = "US/Pacific"),
       ymd_h(2017022823, tz = "US/Pacific")) +
  ylab("Discharge amount (ft^3 per sec)") +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 20)) +
  # add points for defined peaks
  geom_point(data = peaks, aes(as.POSIXct(dt), y),
             color = ifelse(peaks$ros == "ros",
                            "#335978", 
                            ifelse(peaks$ros == "pot-ros",
                                   "#41ab5d", "#bf812d")
             )
  )

grid.arrange(g1, g2, nrow = 2)
dev.off()

# log box plot
ggplot(peaks_df100v2, aes(x = ros, y = log(y))) +
  geom_boxplot(aes(fill = ros), show.legend = FALSE) +
  xlab("ROS Classification") +
  ylab("Streamflow") +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 13, 1))

# log violin plot
ggplot(peaks_df100v2, aes(x = ros, y = log(y))) +
  geom_violin(aes(fill = ros), show.legend = FALSE) +
  xlab("ROS Classification") +
  ylab("Log streamflow (log(ft ^ 3 per sec))") +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, 1)) +
  scale_fill_manual(values = c("#bf812d", "#41ab5d", "#335978")) +
  scale_x_discrete(breaks = c("non-ros", "pot-ros", "ros"),
                   labels = c("Non-ROS", "Potential ROS", "ROS"))



ggplot(filter(peaks_df100v2, ros == "ros"), aes(x = ))

hist(filter(peaks_df100v2, ros == "ros")$y,
     breaks = 10
)
hist(filter(peaks_df100v2, ros == "non-ros")$y,
     breaks = 10
)
hist(filter(peaks_df100v2, ros == "pot-ros")$y,
     breaks = 10
)


## quantile tables
# temp based
ros <- round(quantile(filter(peaks_df100v2, ros == "ros")$y,
                      c(.05, .1, .15, .25, .5, .75, .9, .95)
))
nonros <- round(quantile(filter(peaks_df100v2, ros == "non-ros")$y,
                         c(.05, .1, .15, .25, .5, .75, .9, .95)
))
round(((ros - nonros) / nonros) * 100)

# snowpack based
ros2 <- round(quantile(filter(peaks_df100v3, ros == "ros")$y,
                       c(.05, .1, .15, .25, .5, .75, .9, .95)
))
nonros2 <- round(quantile(filter(peaks_df100v3, ros == "non-ros")$y,
                          c(.05, .1, .15, .25, .5, .75, .9, .95)
))
round(((ros2 - nonros2) / nonros2) * 100)


# devtools::install_github("lschneider93/rsnodas"), https://github.com/lschneider93/rsnodas


