# libraries
library(tidyverse)
library(data.table)
library(imputeTS)
library(ggplot2)

# load peak data
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf.rds")

# subset to nevada -- use third peak from bottom since it's the only one that
# was classified as ROS
nv_peaks <- filter(peak_data_dt, state == "NV" & id == 10349300)

# snotel data
snotel_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                      states[i], ".RDS"))
  snotel_all <- rbind(snotel_all, x)
}

# take median of snotel conditions for previous five days for each date/huc
temp <- snotel_all[date %in% seq(as.Date("2017-02-10") - 5,
                                 as.Date("2017-02-10"), by = "day")
                   & huc == 16050102]

# how many unique snotels are there?
length(unique(temp$id))

# median measurements by id
summs <- temp[, .(temp = mean(temp_degc, na.rm = TRUE),
         prec = mean(prec, na.rm = TRUE),
         snow_dep = mean(snow_dep, na.rm = TRUE),
         swe = mean(swe, na.rm = TRUE),
         soil_mp8in = mean(soil_mp8in, na.rm = TRUE),
         soil_mp20in = mean(soil_mp20in, na.rm = TRUE),
         melt = mean(melt, na.rm = TRUE),
         elev = mean(elev, na.rm = TRUE)
         ), by = id]
summs

sapply(summs[, -1], median)
