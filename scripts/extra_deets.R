library(tidyverse)

# figure out how many unique streamgages there are left
usgs <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
length(unique(usgs$id))

# get hucs for unique snotel stations
snotel <- read.csv("data-raw/snotel_id.csv")
snotel_hucs <- snotel |>
  mutate(huc8 = as.numeric(substr(str_extract_all(huc, "\\((\\d+)\\)"), 2, 9)))

# how many snotel stations are in hucs with streamgages?
sum(snotel_hucs$huc8 %in% usgs$huc) # 636

# how many streamgages in final dataset had flood stage estimated? 4977
ests <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
usgs_est <- filter(ests, site_no %in% unique(usgs$id)) |>
  select(site_no, est) |>
  filter(est == TRUE)
filter(usgs, id %in% usgs_est$site_no) |>
  nrow()

# how many ros and non-ros peaks are in the final data frame
peaks_sf |>
  group_by(ros) |>
  summarize(n())
