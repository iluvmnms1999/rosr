library(data.table)

# read in all the files with ros class using melt method and snotel precip
melt_az <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_AZ.RDS")
melt_ca <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_CA.RDS")
melt_co <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_CO.RDS")
melt_id <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_ID.RDS")
melt_mt <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_MT.RDS")
melt_nm <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_NM.RDS")
melt_nv <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_NV.RDS")
melt_or <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_OR.RDS")
melt_ut <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_UT.RDS")
melt_wa <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_WA.RDS")
melt_wy <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_WY.RDS")

# read in df with all peaks and their baseflows using median method
base_med <- readRDS("data-raw/peaks_fin/peaks_base_med.RDS")

# add baseflow to all dfs
x <- dplyr::left_join(melt_az, base_med, by = c("state", "id", "dt"))
# get rid of duplicate peakflow
temp <- x[, -7]
names(temp)[4] <- "peakflow"
temp <- temp[, c(1, 2, 5, 3, 4, 7, 6)]
temp
saveRDS(temp, "data-raw/ros_class/huc_match/melt_snotel/add_base_med/ms_base_AZ.RDS")
