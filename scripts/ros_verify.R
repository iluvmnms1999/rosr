usgs <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
snotel <- readRDS("data-raw/snotel/snotel_huc.RDS")

sum(usgs$state == "NV", na.rm = TRUE)
sum(snotel$state == "NV", na.rm = TRUE)

nv_melt_sn <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ros_peaks_ms_NV.RDS")
head(nv_melt_sn)
sum(nv_melt_sn$ros == "ros") # 34
length(unique(nv_melt_sn$id)) # 57/139
ros <- nv_melt_sn[ros == "ros"]
nros <- nv_melt_sn[ros == "non-ros"]
summary(ros$y)
summary(nros$y)
nros[which(y > 2475)]
ros[which(y > 3010)] # the dates look right

nv_swe_sn <- readRDS("data-raw/ros_class/huc_match/swe_snotel/ros_peaks_ss_NV.RDS")
head(nv_swe_sn)
sum(nv_swe_sn$ros == "ros") # 84

nv_temp_split <- readRDS("data-raw/ros_class/huc_match/temp_split/ros_peaks_ts_NV.RDS")
head(nv_temp_split)
sum(nv_temp_split$ros == "ros") # 48

nv_temp <- readRDS("data-raw/ros_class/huc_match/temp/ros_peaks_t_NV.RDS")
head(nv_temp)
sum(nv_temp$ros == "ros") # 50

# edit
# are you working now
