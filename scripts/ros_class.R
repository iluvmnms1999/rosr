## classify peaks as ROS or not

usgs_huc <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
snotel_huc <- readRDS("data-raw/snotel/snotel_huc.RDS")
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")

## get melt
swe_prec <- read.csv("data-raw/ros_class/swe_prec.csv", header = TRUE)

# sort data frame by station
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
