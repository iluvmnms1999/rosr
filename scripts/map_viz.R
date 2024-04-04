library(ggplot2)
library(usmap)
library(sf)
library(tidyverse)
library(data.table)

states <- us_map(regions = "state")
west <- subset(states, abbr %in% c("NV", "CA", "CO", "ID", "MT", "NM",
                                   "OR", "UT", "WA", "AZ", "WY"))
# get hucs
huc8 <- readRDS("data-raw/wbd/ws_huc8_geom.rds")
huc8_simp <- st_simplify(huc8, dTolerance = 1000)
huc8_filt <- huc8_simp |>
  filter(str_detect(states, "NV|CA|CO|ID|MT|NM|OR|UT|WA|AZ|WY") == TRUE)
huc8_filt$huc8 <- as.double(huc8_filt$huc8)

# get retained peaks
peaks_ref <- readRDS("data-raw/modeling/peak_data_sf.rds")
peaks_ref <- st_as_sf(peaks_ref)

# get all peaks
usgs_huc <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")
peaks <- peaks[, huc := rep(0, nrow(peaks))]
for (i in seq_along(usgs_huc$site_no)) {
  # add huc variable to peak data
  peaks[peaks$id == formatC(usgs_huc$site_no[i],
                            width = 8,
                            format = "d",
                            flag = "0")]$huc <- rep(usgs_huc$huc8[i],
                                                    times = nrow(peaks[peaks$id == formatC(usgs_huc$site_no[i],
                                                                                           width = 8,
                                                                                           format = "d",
                                                                                           flag = "0")]))
}
peaks$id <- as.integer(peaks$id)
usgs_id <- read.csv("data-raw/usgs_id.csv")
names(usgs_id)[1] <- "id"
peaks_all <- peaks |>
  left_join(usgs_id[, c(1, 4:5)], join_by(id))
peaks_all <- sf::st_as_sf(peaks_all, coords = c("dec_long_va", "dec_lat_va"),
                          crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

# get snotel stations corresponding to retained streamgages
# csv with location
snotel_id <- read.csv("data-raw/snotel_id.csv") |>
  mutate(huc8 = as.numeric(substr(str_extract_all(huc, "\\d+\\)$"), 1, 8)))

match <- which(snotel_id$huc8 %in% unique(peaks_ref$huc))
snotel_keep <- snotel_id[match, ]
snotel_sf <- sf::st_as_sf(snotel_keep, coords = c("lon", "lat"),
                         crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

# get all snotels (except for alaska)
snotel_filt <- snotel_id |>
  filter(!(state %in% c("AK", "SD")))
snotel_all <- sf::st_as_sf(snotel_filt, coords = c("lon", "lat"),
                          crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

# state outlines with hucs overlaid (specify "data =")
# https://github.com/tidyverse/ggplot2/issues/2090

## plot western states with huc8 regions and all usgs/snotel stations
pdf("figures/ch2/states_huc_stations_all.pdf", height = 6, width = 6)
par(mfrow = c(1, 2))
ggplot() +
  # geom_sf(data = states, fill = "white", color = "gray") +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_filt, col = "gray50", fill = NA) +
  geom_sf(data = snotel_all, color = "blue", size = 1) +
  geom_sf(data = peaks_all, color = "red3", size = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

## plot western states with huc8 regions and usgs/snotel stations retained
## because they're in the same hucs
pdf("figures/ch2/states_huc_stations_filt.pdf", height = 6, width = 6)
ggplot() +
  # geom_sf(data = states, fill = "white", color = "gray") +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_filt, col = "gray50", fill = NA) +
  geom_sf(data = snotel_sf, color = "blue", size = 1) +
  geom_sf(data = peaks_ref, color = "red3", size = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

## get periods of record
# which stations do we need period of record for?
# stations <- peaks |>
#   select(state, id) |>
#   group_by(id) |>
#   slice(1) |>
#   as.data.frame() |>
#   setDT()
#
# stations <- stations[, por := 0]
#
# # get lengths of period of records for each unique station
# states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
# for (x in seq_along(states)) {
#   # get all streamflow data
#   rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
#   rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
#   rhv_all <- rbind(rhv_tot, rhv_miss)
#   data.table::setDT(rhv_all)
#   rhv_all$id <- as.integer(rhv_all$id)
#
#   # subset state-specific id's needed
#   stat_temp <- stations[state == states[x]]
#
#   por <- c()
#   for (i in seq_len(nrow(stat_temp))) {
#     temp <- rhv_all[id == stat_temp$id[i]]
#     por[i] <- difftime(temp$datetime[nrow(temp)], temp$datetime[1],
#                        unit = "days") / 365
#   }
#   stations[state == states[x]]$por <- por
# }
#
# saveRDS(stations, "data-raw/por_length.rds")
#
# # how many surges at each station?
# surges_count <- peaks[, .(surge_count = .N,
#                           huc = max(huc)), by = id]
#
# ros_count <- peaks[ros == "ros", .(ros_count = .N), by = id]
#
# stat_surge <- left_join(stations, surges_count, by = join_by(id))
# stat_tot <- left_join(stat_surge, ros_count, by = join_by(id))
# stat_tot[is.na(stat_tot)] <- 0
# names(stat_tot)[6] <- "huc8"
# stat_tot <- stat_tot[, -3]
#
# stat_huc <- left_join(stat_tot, huc8_filt, by = join_by(huc8))
# stat_huc <- stat_huc[, -7]
# stat_huc <- st_as_sf(stat_huc)
#
# saveRDS(stat_huc, "data-raw/por_ros_surge.rds")
#

stat_huc <- readRDS("data-raw/por_ros_surge.rds")

# summarize by huc
huc_sums <- stat_huc |>
  reframe(surge_prop = surge_count / por,
            ros_prop_tot = ros_count / surge_count,
            ros_prop_year = ros_count / por,
            huc8 = huc8,
            geometry = geometry) |>
  group_by(huc8) |>
  summarize(surge = ifelse(n() == 1, surge_prop, median(surge_prop)),
            ros_tot = mean(ros_prop_tot),
            ros_year = mean(ros_prop_year)) |>
  left_join(huc8_filt, by = join_by(huc8)) |>
  select(-5) |>
  setDT() |>
  st_as_sf()

# saveRDS(huc_sums, "data-raw/huc_ros_surgesums.rds")

# huc_sums <- readRDS("data-raw/huc_ros_surgesums.rds")

## choropleth map of # of surges per year in each huc
# help: https://community.appliedepi.org/t/how-to-overlay-a-choropleth-map-on-a-base-map/1365
pdf("figures/ch2/huc_surge_overlay.pdf", height = 4, width = 4)
ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_filt, col = "gray50", fill = NA) +
  geom_sf(data = huc_sums, aes(fill = log2(surge)), inherit.aes = FALSE) +
  theme_bw() +
  guides(fill = guide_legend(title = "Surge\n(log2)")) #+
  # ggtitle("# eligible surges per year by HUC") +
  # theme(plot.title = element_text(hjust = 0.5))
dev.off()

## choropleth map of prop of ros surges in each huc
pdf("figures/ch2/huc_rostot_overlay.pdf", height = 4, width = 4)
ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_filt, col = "gray50", fill = NA) +
  geom_sf(data = huc_sums, aes(fill = ros_tot), inherit.aes = FALSE) +
  theme_bw() +
  guides(fill = guide_legend(title = "ROS\nProp.")) +
  scale_fill_gradient(low = "#00441b", high = "#e5f5e0") #+
  # ggtitle("prop. of total surges classified as ROS by HUC") +
  # theme(plot.title = element_text(hjust = 0.5))
dev.off()

## choropleth map of prop of ros surges per year in each huc
pdf("figures/ch2/huc_rosyear_overlay.pdf", height = 4, width = 5)
ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_filt, col = "gray50", fill = NA) +
  geom_sf(data = huc_sums, aes(fill = log2(ros_year)), inherit.aes = FALSE) +
  theme_bw() +
  scale_fill_gradient(low = "#00441b", high = "#e5f5e0") +
  ggtitle("# of surges classified as ROS per year by HUC") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
