library(ggmap)
library(sf)
library(ggplot2)
library(tidyverse)

# get huc8 shapefile
hucs <- readRDS("data-raw/wbd/ws_huc8_geom.rds")

# get just nev hucs
ind <- which(stringr::str_detect(hucs$states, "NV"))
nv_hucs <- hucs[ind,]

# expand bounding boxes of hucs
bbox_df <- as.data.frame(do.call("rbind", lapply(st_geometry(nv_hucs), st_bbox)))
bbox_df <- cbind(nv_hucs$huc8, bbox_df)
head(bbox_df)

# subset left and right nev
left <- bbox_df |>
  filter(xmax < -117)

# plot huc regions over map
ggmap::register_google(key = "AIzaSyBOLz3Omb-9HWFlspMDoCLo652IzITbwRY")
bbox <- setNames(sf::st_bbox(nv_hucs$geometry),
                 c("left", "bottom", "right", "top"))
basemap_streets <- ggmap::get_map(maptype = "roadmap", location = bbox, zoom = 6)
street_map <- ggmap::ggmap(basemap_streets)

print(street_map)

street_map + geom_sf(data = nv_hucs[nv_hucs$huc8 %in% left$`nv_hucs$huc8`,],
                     inherit.aes = FALSE,
                     color = "gray48", fill = NA) #+
  # geom_text(data = nv_hucs,
  #           aes(label = huc8, x = longitude, y = latitude),
  #           hjust = 0, size = 2.5, nudge_x = 0.02, col = "gray48")

street_map + geom_sf(data = nv_hucs[nv_hucs$huc8 == "16050201",],
                     inherit.aes = FALSE,
                     color = "gray48", fill = NA)
