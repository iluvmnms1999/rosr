library(terra)

## re-get prism data for aggregation purposes

# get huc boundaries for cropping
# get watersheds for all states, combine
# regions <- c("10", "11", "13", "14", "15", "16", "17", "18")
# ws_all <- data.frame()
# for (i in seq_along(regions)) {
#   temp <- sf::st_read(
#     dsn = paste0("data-raw/wbd/huc_geoms/WBD_", regions[i], "_HU2_Shape/Shape"),
#     layer = "WBDHU8"
#   )
#   temp_val <- sf::st_make_valid(temp)
#   temp_sub <- select(temp_val, c(states, huc8, geometry))
#   ws_all <- rbind(ws_all, temp_sub)
#
# }
# # remove duplicates
# ws <- ws_all[!duplicated(ws_all[, c("huc8", "geometry")]),]
# save huc8 regions
# saveRDS(ws, "data-raw/wbd/ws_huc8.RDS")

ws <- readRDS("data-raw/wbd/ws_huc8.RDS")
files <- list.files("data-raw/prism/daily_ppt")

df <- data.frame()

prism_ppt_20230101 <- terra::rast("C:/Users/student/Downloads/school/stat_6685/proj/elev/data-raw/prism/daily_ppt/PRISM_ppt_stable_4kmD2_20220101_bil.bil")

for (i in seq_along())
for (i in seq_along(ws$huc8)) {
  huc <- ws$huc8[1]
  prism_ws_rast <- terra::extract(prism_ppt_20230101, vect(ws$geometry[1]),
                                  weights = TRUE,
                                  exact = TRUE, touches = TRUE)
  vals <- c(median(prism_ws_rast[,2], na.rm = TRUE),
            mean(prism_ws_rast[,2], na.rm = TRUE),
            max(prism_ws_rast[,2], na.rm = TRUE),
            sum(prism_ws_rast[,2], na.rm = TRUE)
  )
}
