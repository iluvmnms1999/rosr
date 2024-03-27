library(tidyverse)

peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")
peaks_fin <- readRDS("data-raw/modeling/peak_data_sf.rds")
usgs <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")

# prep
peaks$id <- as.integer(peaks$id)

usgs_est <- usgs |>
  filter(est == TRUE)

peaks_est <- peaks |>
  filter(id %in% usgs_est$site_no)

peaks_est <- peaks_fin |>
  filter(id %in% usgs_est$site_no)
