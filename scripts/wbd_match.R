## matching up hucs to usgs and snotel stations using the wbd

# library(sf)
# library(tidyverse)
# library(ggmap)

states <- c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa", "wy")
usgs_full <- readRDS("data-raw/usgs_fs/usgs_fs_alm.RDS")
data.table::setDT(usgs_full)
# add empty column for huc number to go in
usgs_huc <- usgs_full[, huc8 := rep("NA", length = nrow(usgs_full))]

for (i in seq_along(states)) {
  # read in huc8 shapefile
  temp <- sf::st_read(
    dsn = paste0("data-raw/wbd/", states[i], "_huc8"),
    layer = paste0("wbdhu8_a_", states[i])
  )
  temp1 <- sf::st_make_valid(temp)

  # subset usgs file on specific state
  usgs <- usgs_full[usgs_full$state == toupper(states[i]),]

  # find which huc usgs station is in
  usgs_sf <- sf::st_as_sf(usgs, coords = c("dec_long_va", "dec_lat_va"),
                          crs = "NAD83", remove = FALSE)
  huc_lst <- sf::st_intersects(usgs_sf, temp1)
  huc_ind <- unlist(huc_lst)
  huc_vec <- temp1[huc_ind, 11]
  usgs_huc[state == toupper(states[i])]$huc8 <- huc_vec$huc8
}
# saveRDS(usgs_huc, "data-raw/usgs_fs/usgs_huc.RDS")

## Get hucs for SNOTEL
states <- c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa", "wy")
snotel_full <- read.csv("data-raw/snotel_id.csv", header = TRUE)
data.table::setDT(snotel_full)
snotel_huc <- snotel_full[, huc8 := rep("NA", length = nrow(snotel_full))]

for (i in seq_along(states)) {
  # read in huc8 shapefile
  temp <- sf::st_read(
    dsn = paste0("data-raw/wbd/", states[i], "_huc8"),
    layer = paste0("wbdhu8_a_", states[i])
  )
  temp1 <- sf::st_make_valid(temp)

  # subset usgs file on specific state
  snotel <- snotel_full[snotel_full$state == toupper(states[i]),]

  # find which huc usgs station is in
  snotel_sf <- sf::st_as_sf(snotel, coords = c("lon", "lat"),
                          crs = "NAD83", remove = FALSE)
  huc_lst <- sf::st_intersects(snotel_sf, temp1)
  huc_ind <- unlist(huc_lst)
  huc_vec <- temp1[huc_ind, 11]
  snotel_huc[state == toupper(states[i])]$huc8 <- huc_vec$huc8
}
snotel_huc1 <- snotel_huc[snotel_huc$state %in% toupper(states),]

saveRDS(snotel_huc1, "data-raw/snotel/snotel_huc.RDS")

## mapping for verification
# mapping help: https://waterdata.usgs.gov/blog/beyond-basic-mapping/
# double check watershed region:
#     https://water.usgs.gov/wsc/cat/16050201.html#.html
# library(ggmap)
# # huc8 map
# ggmap::register_google(key = "AIzaSyBOLz3Omb-9HWFlspMDoCLo652IzITbwRY")
# bbox <- setNames(st_bbox(nev8$geometry[nev8$huc8 == 16050201]),
#                  c("left", "bottom", "right", "top"))
# basemap_streets <- get_map(maptype = "roadmap", location = bbox, zoom = 9)
# street_map <- ggmap(basemap_streets)
#
# print(street_map)
#
# street_map + geom_sf(data = nev_shp,
#                      inherit.aes = FALSE,
#                      color = "black", fill = NA) +
#   #geom_sf(data = snotel_ref, inherit.aes = FALSE, color = "blue") +
#   geom_sf(data = usgs_ref, inherit.aes = FALSE, color = "red") +
#   geom_text(data = usgs_ref,
#             aes(label = site_num, x = longitude, y = latitude),
#             hjust = 0, size = 2.5, nudge_x = 0.02, col = "gray48")

# check usgs hucs for accuracy
usgs <- read.csv("data-raw/usgs_hucs_web.csv", header = TRUE)
usgs <- usgs[, 2:3]
colnames(usgs) <- c("site_no", "web_huc8")
data.table::setDT(usgs)

usgs_og <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
data.table::setDT(usgs_og)

usgs_nodup <- unique(usgs, by = "site_no")

usgs_comb <- dplyr::left_join(usgs_og, usgs_nodup, by = c("site_no"))

sum(is.na(usgs_comb$web_huc8))

# make sure usgs hucs line up
vec <- c()
for (i in seq_along(usgs_comb$huc8)) {
  vec[i] <- usgs_comb$huc8[i] == as.character(usgs_comb$web_huc8[i])
}

nrow(usgs_comb[which(vec == FALSE),]) # 37 hucs don't agree -- 10 are NA's

