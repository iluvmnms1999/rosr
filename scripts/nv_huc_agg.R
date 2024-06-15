# libraries
library(tidyverse)

# load peak data
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf.rds")

# subset to nevada
nv_peaks <- filter(peak_data_dt, state == "NV" & id == 10349300)

# see how many SNOTELS are in that huc
nv_bf <- readRDS("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_NV.RDS")
# HUC has 3 SNOTELS
filter(nv_bf, huc == 16050102)

# get id's for HUC regions
