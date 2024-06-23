library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
# Next four packages all for getting the raster data correct.
library(terra)
library(stars)
library(sf)
library(gstat)

# load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")

# reformat peak data so we have the same variables
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros, snowdep = snow_dep_av, prec = prec_max,
                              swe = swe_av, temp = temp_degc_med)]

# fit gam
gam_obj <- mgcv::gam(log(mult) ~
                       s(temp) +
                       s(snowdep) +
                       s(prec) +
                       s(swe) +
                       s(log(med_bf)) +
                       s(lat, lon, bs = 'sos', k = 25),
                     data = peak_data)

# Plot silenty returns the data. Select = -1
# makes it so I don't actually show the
# default plots.
tdata <- plot(gam_obj, select = -1)

# Access each set of data in a list. I am going to show how to
# handle the raster data (layer 6) and you can infer the rest of the layers.
# Note the list positions will change if you mess around with the order
# of the terms in your GAM.
t_6 <- tdata[[6]]

# Create a data frame with the coordinates and values.
tdf <- data.frame(lon = t_6$lo, lat = t_6$la,
                  value = t_6$fit[, 1]) |>
  tidyr::drop_na()

# Convert to a spatial data frame.
tdf_sf <- sf::st_as_sf(tdf, coords = c("lon", "lat"), crs = 4326)

# Read in a template raster (obtained from 4km prism elevation)
# - https://www.prism.oregonstate.edu/normals/
prism_template <- terra::rast("data-raw/prism/PRISM_us_dem_4km_asc/PRISM_us_dem_4km_asc.asc")

# Create a map of states and project them to the same extent as prism
states <- maps::map("state", fill = TRUE, plot = FALSE)
states <- st_as_sf(states) |>
  dplyr::filter(ID %in% c("washington", "oregon", "california",
                   "nevada", "idaho", "utah", "arizona",
                   "montana", "wyoming", "colorado",
                   "new mexico"))
states <- st_transform(states, crs = crs(prism_template))

# Transform our spatial points as well.
tdf_sf <- st_transform(tdf_sf, crs = crs(prism_template))

# Crop prism data according to a bounding box of states. Because
# state geometry is whack, we create a geometry out of the bounding box only.
bounding_box <- st_bbox(states) %>% st_as_sfc()
prism_crop <- crop(prism_template, bounding_box)
tdf_sf_2 <- st_intersection(tdf_sf, bounding_box)

# Now aggregate the PRISM data to be similar in resolution to our points.
prism_crop_2 <- aggregate(prism_crop, 30)

# Now use inverse distance weighting to interpolate between the points.
new_rast <- gstat::idw(value ~ 1, locations = tdf_sf_2,
                       newdata = st_as_stars(prism_crop_2))

# FINALLY, plot the rasterized points on a plot with the states.
# You can mess around with the layers to make this look nicer. I recommend
# a more variable color scheme!
ggplot() +
  geom_stars(data = new_rast) +
  geom_sf(data = states, fill = NA) +
  xlab("") +
  ylab("") +
  scale_fill_gradient(breaks = waiver(), n.breaks = 6, low = "#d0d1e6", high = "#016450",
                      # labels = rev(c(4, 2, 1, 0.5, 0.25, 0.13, 0.06, 0.03)),
                      na.value = "transparent") +
  theme_bw()
