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


## GET PREC
df <- data.frame()

# change wd to elev
files <- list.files(paste0("data-raw/prism/daily_ppt"))
bils <- files[grep(".bil$", files)]
dates <- substr(bils, 24, 31)
for (i in seq_along(bils)) {
  prism_ppt <- terra::rast(paste0("data-raw/prism/daily_ppt/", bils[i]))
  for (j in seq_along(ws$huc8)) {
    prism_rast <- terra::extract(prism_ppt, vect(ws$geometry[j]),
                                    weights = TRUE,
                                    exact = TRUE, touches = TRUE)
    vals <- c(dates[i],
              ws$huc8[j],
              median(prism_rast[,2], na.rm = TRUE),
              mean(prism_rast[,2], na.rm = TRUE),
              max(prism_rast[,2], na.rm = TRUE),
              sum(prism_rast[,2], na.rm = TRUE)
    )
    df <- rbind(df, vals)
  }
}
names(df) <- c("date", "huc8", "median", "mean", "max", "sum")
saveRDS("data-raw/prism/ppt_summaries.rds")
