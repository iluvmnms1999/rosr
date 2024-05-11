library(tidyverse)

# figure out how many unique streamgages there are left
usgs <- readRDS("data-raw/modeling/peak_data_sf.rds")
length(unique(usgs$id))

# get hucs for unique snotel stations
snotel <- read.csv("data-raw/snotel_id.csv")
snotel_hucs <- snotel |>
  mutate(huc8 = as.numeric(substr(str_extract_all(huc, "\\((\\d+)\\)"), 2, 9)))

# how many snotel stations are in hucs with streamgages?
sum(snotel_hucs$huc8 %in% usgs$huc) # 636
