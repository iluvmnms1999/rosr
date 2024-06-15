# libraries
library(tidyverse)

# load peak data
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf.rds")

# subset to nevada
nv_peaks <- filter(peak_data_dt, state == "NV" & id == 10349300)

# see how many SNOTELS are in that huc
nv_bf <- readRDS("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_NV.RDS")
# HUC has 3 SNOTELS
huc_lev <- filter(nv_bf, huc == 16050102)

# get id's for HUC regions
unique(huc_lev$id)

# peak to get stats for -- peak that happened on 2017-02-10 at gage 10349300 (ROS)
## filter to get 5 day window for each HUC to take median of
# 340:NV:SNTL
sntl_340 <- filter(huc_lev, id == "340:NV:SNTL" & date %in% seq(as.Date("2017-02-05"),
                                                                as.Date("2017-02-10"),
                                                                by = "day"))
summ_340 <- sapply(sntl_340, median)

# 1242:NV:SNTL
sntl_1242 <- filter(huc_lev, id == "1242:NV:SNTL" & date %in% seq(as.Date("2017-02-05"),
                                                                as.Date("2017-02-10"),
                                                                by = "day"))
summ_1242 <- sapply(sntl_1242, median)

# 652:NV:SNTL
sntl_652 <- filter(huc_lev, id == "652:NV:SNTL" & date %in% seq(as.Date("2017-02-05"),
                                                                as.Date("2017-02-10"),
                                                                by = "day"))
summ_652 <- sapply(sntl_652, median)

# sum of the row medians are wrong... not hard to see what it should be though
summs <- data.frame(summ_340, summ_1242, summ_652)[-(1:2),]
summs$overall_med <- apply(summs, 1, median)
