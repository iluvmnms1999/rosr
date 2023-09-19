library(sf)
library(tidyverse)
library(ggmap)

# finding points within sf polygons
# https://mattherman.info/blog/point-in-poly/

# read in huc8 shapefile
az8 <- sf::st_read(
  dsn = "data-raw/wbd/az_huc8",
  layer = "wbdhu8_a_az"
)

# read in data with ids/states for usgs and snotel stations
states <- c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa", "wy")
usgs <- readRDS("data-raw/usgs_fs/usgs_fs_alm.RDS")
usgs <- usgs[usgs$state %in% toupper(states),]

snotel <- read.csv("data-raw/snotel_id.csv", header = TRUE)
snotel <- snotel[snotel$state %in% toupper(states),]

## USGS - huc8
# no data for 10309010
# all these guys have same huc8 (16050201) and are in wb #62 - upper carson
usgs_sf <- sf::st_as_sf(usgs, coords = c("dec_long_va", "dec_lat_va"), crs = "NAD83",
                    remove = FALSE)
usgs_lst <- sf::st_intersects(usgs_sf, az8)

## SNOTEL - huc12
snotel_sf <- st_as_sf(snotel, coords = c("longitude", "latitude"),
                      crs = "NAD83", remove = FALSE)
snotel_ref <- snotel_sf[regexpr(snotel_sf$huc,
                                pattern = "^16050201[[:alnum:]]+$") > 0, ]
snotel_lst <- st_intersects(snotel_ref, ca_nv10)

## SHP FILE
nev_shp <- nev10$geometry[nev10$huc8 == 16050201]
ca_nevhucs10 <- ca_nv10[c(6, 328, 480, 1204),]
nev8[62,]

head(ca_nevhucs10)

## mapping for verification
# mapping help: https://waterdata.usgs.gov/blog/beyond-basic-mapping/
# double check watershed region:
#     https://water.usgs.gov/wsc/cat/16050201.html#.html
library(ggmap)
# huc8 map
ggmap::register_google(key = "AIzaSyBOLz3Omb-9HWFlspMDoCLo652IzITbwRY")
bbox <- setNames(st_bbox(nev8$geometry[nev8$huc8 == 16050201]),
                 c("left", "bottom", "right", "top"))
basemap_streets <- get_map(maptype = "roadmap", location = bbox, zoom = 9)
street_map <- ggmap(basemap_streets)

print(street_map)

street_map + geom_sf(data = nev_shp,
                     inherit.aes = FALSE,
                     color = "black", fill = NA) +
  #geom_sf(data = snotel_ref, inherit.aes = FALSE, color = "blue") +
  geom_sf(data = usgs_ref, inherit.aes = FALSE, color = "red") +
  geom_text(data = usgs_ref,
            aes(label = site_num, x = longitude, y = latitude),
            hjust = 0, size = 2.5, nudge_x = 0.02, col = "gray48")
