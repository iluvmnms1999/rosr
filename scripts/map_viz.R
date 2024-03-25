library(ggplot2)
library(usmap)
library(sf)
library(tidyverse)

states <- us_map(regions = "state")
west <- subset(states, abbr %in% c("NV", "CA", "CO", "ID", "MT", "NM",
                                   "OR", "UT", "WA", "AZ", "WY"))
huc8 <- readRDS("data-raw/wbd/ws_huc8_geom.rds")
huc8_simp <- st_simplify(huc8, dTolerance = 1000)
huc8_filt <- huc8_simp |>
  filter(str_detect(states, "NV|CA|CO|ID|MT|NM|OR|UT|WA|AZ|WY") == TRUE)

huc4 <- readRDS("data-raw/wbd/ws_huc4_geom.rds")
huc4_simp <- st_simplify(huc4, dTolerance = 1000)

peaks <- readRDS("data-raw/modeling/peak_data_sf.rds")
peaks <- st_as_sf(peaks)

# state outlines with hucs overlaid (specify "data =")
# https://github.com/tidyverse/ggplot2/issues/2090

## plot western states with huc8 regions overlaid
pdf("figures/states_huc_overlay.pdf", height = 6, width = 6)
ggplot() +
  # geom_sf(data = states, fill = "white", color = "gray") +
  geom_sf(data = west, colour = "black", lwd = 1) +
  geom_sf(data = huc8_filt, fill = "blue", alpha = 0.3) +
  # geom_sf(data = huc4_simp, fill = NA, color = "blue", lwd = 0.75)
  # geom_sf(data = peaks, color = "red", size = 1) +
  ggtitle("Western States with HUC 8 regions overlaid") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


## choropleth map of # of surges per year in each huc
# which stations do we need period of record for?
stations <- peaks |>
  select(state, id) |>
  group_by(id) |>
  slice(1) |>
  as.data.frame() |>
  setDT()

# get lengths of period of records for each unique station
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
for (x in seq_along(states)) {
  # get all streamflow data
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)
  rhv_all$id <- as.integer(rhv_all$id)

  # subset state-specific id's needed
  stat_temp <- stations[state == states[x]]

  por <- c()
  for (i in seq_along(stat_temp)) {
    temp <- rhv_all[id == stat_temp$id[i]]
    por[i] <- year(temp$datetime[nrow(temp)]) - year(temp$datetime[1])
  }
  stations[state == states[x]]$por <- por
}
