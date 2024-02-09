## libraries
library(data.table)
library(imputeTS)
library(ggplot2)

## make data matrix --------------------------------------------------------
# peaks data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
peaks_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel",
                      "/add_base_med_ref/ms_baseref_", states[i], ".RDS"))
  peaks_all <- rbind(peaks_all, x)
}
# get date from datetime
peaks_all[, date := as.Date(dt)]
# get dates of all peaks to look at snotel measurements for
dates <- peaks_all[, .(date, huc)]

# snotel data
snotel_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                      states[i], ".RDS"))
  snotel_all <- rbind(snotel_all, x)
}

# take median of snotel conditions for previous five days for each date/huc
for (i in seq_len(nrow(dates))) {
  temp <- snotel_all[date %in% seq(dates$date[i] - 5,
                                   dates$date[i], by = "day")
                     & huc == dates$huc[i]]
  dates$temp_degc_av[i] <- median(temp[, mean(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$temp_degc_med[i] <- median(temp[, median(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$temp_degc_min[i] <- median(temp[, min(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$temp_degc_max[i] <- median(temp[, max(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$prec_av[i] <- median(temp[, mean(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_med[i] <- median(temp[, median(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_min[i] <- median(temp[, min(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_max[i] <- median(temp[, max(prec, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_av[i] <- median(temp[, mean(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_med[i] <- median(temp[, median(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_min[i] <- median(temp[, min(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_max[i] <- median(temp[, max(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$swe_av[i] <- median(temp[, mean(swe, na.rm = TRUE), by = id]$V1)
  dates$swe_med[i] <- median(temp[, median(swe, na.rm = TRUE), by = id]$V1)
  dates$swe_min[i] <- median(temp[, min(swe, na.rm = TRUE), by = id]$V1)
  dates$swe_max[i] <- median(temp[, max(swe, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_av[i] <- median(temp[, mean(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_med[i] <- median(temp[, median(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_min[i] <- median(temp[, min(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_max[i] <- median(temp[, max(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_av[i] <- median(temp[, mean(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_med[i] <- median(temp[, median(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_min[i] <- median(temp[, min(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_max[i] <- median(temp[, max(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$melt_av[i] <- median(temp[, mean(melt, na.rm = TRUE), by = id]$V1)
  dates$melt_med[i] <- median(temp[, median(melt, na.rm = TRUE), by = id]$V1)
  dates$melt_min[i] <- median(temp[, min(melt, na.rm = TRUE), by = id]$V1)
  dates$melt_max[i] <- median(temp[, max(melt, na.rm = TRUE), by = id]$V1)
  dates$elev_av[i] <- median(temp[, mean(elev, na.rm = TRUE), by = id]$V1)
  dates$elev_med[i] <- median(temp[, median(elev, na.rm = TRUE), by = id]$V1)
  dates$elev_min[i] <- median(temp[, min(elev, na.rm = TRUE), by = id]$V1)
  dates$elev_max[i] <- median(temp[, max(elev, na.rm = TRUE), by = id]$V1)
}
# save data table
is.na(dates) <- sapply(dates, is.infinite)
saveRDS(dates, "data-raw/modeling/snotel_av_med.rds")

# which STATIONS have smp reported for at least part of their period of record?
stat_soilmp <- snotel_all[, .(total = .N,
                              eight_perc = round((.N - sum(is.na(soil_mp8in)))
                                                 / .N, 3),
                              twenty_perc = round((.N - sum(is.na(soil_mp20in)))
                                                  / .N, 3)),
                          by = id]

# which HUCS have smp reported for at least part of their period of record?
huc_soilmp <- snotel_all[, .(total = .N,
                             eight_perc = round((.N - sum(is.na(soil_mp8in)))
                                                / .N, 3),
                             twenty_perc = round((.N - sum(is.na(soil_mp20in)))
                                                 / .N, 3),
                             state = state),
                         by = huc]#[eight == 0 & twenty == 0]

# connect smp findings to peaks
peaks_sel <- peaks_all[, ":="(smp = ifelse(huc %in% huc_soilmp$huc[which(huc_soilmp$eight_perc == 0
                                                                     & huc_soilmp$twenty_perc == 0)],
                                           0, 1),
                              mult = peakflow / base_med)][base_med > 0]

# join snotel and peaks by huc and date
snotel_smp <- readRDS("data-raw/modeling/snotel_av_med.rds")

peak_data_dt <- dplyr::left_join(peaks_sel, snotel_smp, by = c("date", "huc"),
                                 multiple = "any")
# final adjustments
peak_data_dt[, ros_num := ifelse(ros == "ros", 1, 0)]

# add lat long to df
usgs <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
colnames(usgs)[2] <- "id"
peak_data_dt$id <- as.integer(peak_data_dt$id)

peak_data_fin <- dplyr::left_join(peak_data_dt, usgs[, .(id, dec_lat_va, dec_long_va)],
                                 by = join_by(id))

saveRDS(peak_data_fin, "data-raw/modeling/peak_data_dt.rds")


# GAM ---------------------------------------------------------------------
library(mgcv)
library(dplyr)

# load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_dt.rds")

# with smp
mod_gam1 <- mgcv::gam(log(mult) ~
                        s(temp_degc_av) +
                        s(temp_degc_med) +
                        s(snow_dep_av) +
                        s(prec_max) +
                        s(soil_mp8in_av) +
                        s(soil_mp8in_max) +
                        s(melt_av) +
                        s(elev_av) +
                        s(swe_av) +
                        s(swe_med) + # 62.6
                        s(dec_lat_va, dec_long_va, bs = 'sos', k = 100) # increases to 77.1
                      ,
                      data = peak_data_dt)
summary(mod_gam1)
plot(mod_gam1)

# without smp
mod_gam2 <- mgcv::gam(log(mult) ~
                        s(temp_degc_av) +
                        s(temp_degc_med) +
                        s(snow_dep_av) +
                        s(prec_av) +
                        s(prec_max) +
                        s(prec_med) +
                        s(melt_av) +
                        s(elev_av) +
                        s(swe_av) +
                        smp + # 42.5
                        s(dec_lat_va, dec_long_va, bs = 'sos', k = 100) # increases to 63.5
                      ,
                      data = peak_data_dt)
summary(mod_gam2)
plot(mod_gam2)


