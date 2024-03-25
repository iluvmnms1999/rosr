## libraries
library(data.table)
library(imputeTS)
library(ggplot2)

## make data matrix --------------------------------------------------------
# peaks data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
peaks_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel",
                      "/add_base_med_ref/ms_baseref_", states[i], ".RDS"))
  peaks_all <- rbind(peaks_all, x)
}
# get date from datetime
peaks_all[, date := as.Date(dt)]
# get dates of all peaks to look at snotel measurements for
dates <- peaks_all[, .(date, huc)]

# snotel data
snotel_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                      states[i], ".RDS"))
  snotel_all <- rbind(snotel_all, x)
}

# take median of snotel conditions for previous five days for each date/huc
for (i in seq_len(nrow(dates))) {
  temp <- snotel_all[date %in% seq(dates$date[i] - 5,
                                   dates$date[i], by = "day")
                     & huc == dates$huc[i]]
  dates$temp_degc_av[i] <- median(temp[, mean(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$temp_degc_med[i] <- median(temp[, median(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$temp_degc_min[i] <- median(temp[, min(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$temp_degc_max[i] <- median(temp[, max(temp_degc, na.rm = TRUE), by = id]$V1)
  dates$prec_av[i] <- median(temp[, mean(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_med[i] <- median(temp[, median(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_min[i] <- median(temp[, min(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_max[i] <- median(temp[, max(prec, na.rm = TRUE), by = id]$V1)
  dates$prec_sum[i] <- median(temp[, sum(prec, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_av[i] <- median(temp[, mean(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_med[i] <- median(temp[, median(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_min[i] <- median(temp[, min(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$snow_dep_max[i] <- median(temp[, max(snow_dep, na.rm = TRUE), by = id]$V1)
  dates$swe_av[i] <- median(temp[, mean(swe, na.rm = TRUE), by = id]$V1)
  dates$swe_med[i] <- median(temp[, median(swe, na.rm = TRUE), by = id]$V1)
  dates$swe_min[i] <- median(temp[, min(swe, na.rm = TRUE), by = id]$V1)
  dates$swe_max[i] <- median(temp[, max(swe, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_av[i] <- median(temp[, mean(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_med[i] <- median(temp[, median(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_min[i] <- median(temp[, min(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp8in_max[i] <- median(temp[, max(soil_mp8in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_av[i] <- median(temp[, mean(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_med[i] <- median(temp[, median(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_min[i] <- median(temp[, min(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$soil_mp20in_max[i] <- median(temp[, max(soil_mp20in, na.rm = TRUE), by = id]$V1)
  dates$melt_av[i] <- median(temp[, mean(melt, na.rm = TRUE), by = id]$V1)
  dates$melt_med[i] <- median(temp[, median(melt, na.rm = TRUE), by = id]$V1)
  dates$melt_min[i] <- median(temp[, min(melt, na.rm = TRUE), by = id]$V1)
  dates$melt_max[i] <- median(temp[, max(melt, na.rm = TRUE), by = id]$V1)
  dates$elev_av[i] <- median(temp[, mean(elev, na.rm = TRUE), by = id]$V1)
  dates$elev_med[i] <- median(temp[, median(elev, na.rm = TRUE), by = id]$V1)
  dates$elev_min[i] <- median(temp[, min(elev, na.rm = TRUE), by = id]$V1)
  dates$elev_max[i] <- median(temp[, max(elev, na.rm = TRUE), by = id]$V1)
}
# save data table
is.na(dates) <- sapply(dates, is.infinite)
saveRDS(dates, "data-raw/modeling/snotel_av_med.rds")

# which STATIONS have smp reported for at least part of their period of record?
stat_soilmp <- snotel_all[, .(total = .N,
                              eight_perc = round((.N - sum(is.na(soil_mp8in)))
                                                 / .N, 3),
                              twenty_perc = round((.N - sum(is.na(soil_mp20in)))
                                                  / .N, 3)),
                          by = id]

# which HUCS have smp reported for at least part of their period of record?
huc_soilmp <- snotel_all[, .(total = .N,
                             eight_perc = round((.N - sum(is.na(soil_mp8in)))
                                                / .N, 3),
                             twenty_perc = round((.N - sum(is.na(soil_mp20in)))
                                                 / .N, 3),
                             date = date),
                         by = huc]#[eight == 0 & twenty == 0]

# connect smp findings to peaks
peaks_sel <- peaks_all[, ":="(smp = ifelse(huc %in% huc_soilmp$huc[which(huc_soilmp$eight_perc == 0
                                                                         & huc_soilmp$twenty_perc == 0)],
                                           0, 1),
                              mult = peakflow / base_med)][base_med > 0]

# join snotel and peaks by huc and date
snotel_smp <- readRDS("data-raw/modeling/snotel_av_med.rds")

peak_data_dt <- dplyr::left_join(peaks_sel, snotel_smp, by = c("date", "huc"),
                                 multiple = "any")
# final adjustments
peak_data_dt[, ros_num := ifelse(ros == "ros", 1, 0)]

# add lat long to df
usgs <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
colnames(usgs)[2] <- "id"
peak_data_dt$id <- as.integer(peak_data_dt$id)

peak_data_fin <- dplyr::left_join(peak_data_dt, usgs[, .(id, dec_lat_va, dec_long_va)],
                                  by = dplyr::join_by(id))

# assign each id to random number between 1 and 10 for cv purposes
cv <- data.frame(id = unique(peak_data_fin$id),
                 cv = sample(1:10, length(unique(peak_data_fin$id)),
                             replace = TRUE))

peaks_cv <- dplyr::left_join(peak_data_fin, cv, by = "id")

# add huc 4 var
peaks_cv$huc4 <- substr(peaks_cv$huc, 1, 4)

# make geometries
ws <- readRDS("data-raw/wbd/ws_huc4_geom.rds")
peaks_sf <- sf::st_as_sf(peaks_cv, coords = c("dec_long_va", "dec_lat_va"),
                         crs = sf::st_crs(ws)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

saveRDS(peaks_sf, "data-raw/modeling/peak_data_sf.rds")

# Baseline model (nothing) ------------------------------------------------
# 10 fold cross-validation for non-regionalized gam
global_mean_preds <- rep(as.numeric(NA), nrow(peak_data_dt))
for (i in 1:10) {
  index <- peak_data_dt$cv == i

  global_mean <- median(log(peak_data_dt$mult[!index]), na.rm = TRUE)

  global_mean_preds[index] <- global_mean
}

global_mean_mse <- mean((log(peak_data_dt$mult) - global_mean_preds)^2, na.rm = TRUE)
global_mean_mae <- median(abs(log(peak_data_dt$mult) - global_mean_preds), na.rm = TRUE)

# GAM ---------------------------------------------------------------------
library(mgcv)
library(dplyr)

# load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf.rds")

# with smp
mod_gam1 <- mgcv::gam(log(mult) ~
                        s(temp_degc_av) +
                        s(temp_degc_med) +
                        s(snow_dep_av) +
                        s(prec_max) +
                        s(soil_mp8in_av) +
                        s(soil_mp8in_max) +
                        s(melt_av) +
                        s(elev_av) +
                        s(swe_av) +
                        s(swe_med) + # 62.6
                        s(base_med) +
                        s(dec_lat_va, dec_long_va, bs = 'sos', k = 100) # increases to 77.1
                      ,
                      data = peak_data_dt)
summary(mod_gam1)
plot(mod_gam1)

# without smp
mod_gam2 <- mgcv::gam(log(mult) ~
                        s(temp_degc_av) +
                        s(temp_degc_med) +
                        s(snow_dep_av) +
                        s(prec_av) +
                        s(prec_max) +
                        s(prec_med) +
                        s(prec_sum) +
                        # s(melt_av) +
                        # s(elev_av) +
                        s(swe_av) +
                        s(base_med) +
                        # smp +
                        s(lat, lon, bs = 'sos', k = 100) # increases to 63.5
                      ,
                      data = peak_data_dt)
summary(mod_gam2)

# 10 fold cross-validation for non-regionalized gam
gam_nr_preds <- rep(as.numeric(NA), nrow(peak_data_dt))
for (i in 1:10) {
  index <- peak_data_dt$cv == i

  gam_nr <- mgcv::gam(log(mult) ~
                        s(temp_degc_av) +
                        # s(temp_degc_med) +
                        s(snow_dep_av) +
                        # s(prec_av) +
                        s(prec_max) +
                        # s(prec_med) +
                        # s(prec_sum) +
                        # s(melt_av) +
                        # s(elev_av) +
                        s(swe_av) +
                        s(base_med) +
                        smp +
                        s(lat, lon, bs = 'sos', k = 25)
                      ,
                      data = peak_data_dt[!index, ])

  gam_nr_preds[index] <- predict(
    gam_nr, peak_data_dt[index, ]
  )
}
# 10-fold cv mse = 2.11 -- not overfitting
gam_nr_capped <- pmax(gam_nr_preds, 0)
mean((log(peak_data_dt$mult) - gam_nr_capped)^2, na.rm = TRUE)
median(abs(log(peak_data_dt$mult) - gam_nr_capped), na.rm = TRUE)


# regional GAMs using remap -----------------------------------------------
library(remap)
library(data.table)
library(dplyr)
library(mgcv)
library(sf)
library(ggplot2)

# read in data
peak_data_dt <- readRDS("data-raw/modeling/peak_data_dt.rds")
ws <- readRDS("data-raw/wbd/ws_huc4_geom.rds")

# get watersheds for all states, combine
# regions <- c("10", "11", "13", "14", "15", "16", "17", "18")
# ws_all <- data.frame()
# for (i in seq_along(regions)) {
#   temp <- sf::st_read(
#     dsn = paste0("data-raw/wbd/huc4_geoms/WBD_", regions[i], "_HU2_Shape/Shape"),
#     layer = "WBDHU4"
#   )
#   temp_val <- sf::st_make_valid(temp)
#   temp_sub <- select(temp_val, c(states, huc4, geometry))
#   ws_all <- rbind(ws_all, temp_sub)
#
# }
# # remove duplicates
# ws <- ws_all[!duplicated(ws_all[, c("huc4", "geometry")]),]
# saveRDS(ws, "data-raw/wbd/ws_huc4_geom.rds")



# add geometries to peak data


# saveRDS(peak_data_dt, "data-raw/modeling/peak_data_dt_sf.rds")

# predefined functions
gam_limit <- function(data, fml) {
  g_model <- mgcv::gam(fml, data = data)
  upper_lim <- max(log(data$mult))

  out <- list(g_model = g_model, upper_lim = upper_lim)
  class(out) <- "gam_limit"
  return(out)
}

predict.gam_limit <- function(object, newobs, se.fit = FALSE) {
  if (nrow(newobs) != 0) {
    if (se.fit) {
      return(predict(object$g_model, newobs, se.fit = TRUE)$se.fit)
    } else {
      preds <- predict(object$g_model, newobs)

      preds[preds < 0] <- 0
      preds[preds > object$upper_lim] <- object$upper_lim

      return(preds)
    }
  }
  return(NULL)
}

# precalculate distances
huc4_dist <- remap::redist(peak_data_dt, ws, region_id = huc4)

# Initialize predictions
gam_huc4_preds <- rep(as.numeric(NA), nrow(peak_data_dt))

# Formula for regional GAMs
gam_huc4_fml <- log(mult) ~
  s(temp_degc_av) +
  s(temp_degc_med) +
  s(snow_dep_av) +
  s(prec_av) +
  s(prec_max) +
  s(prec_med) +
  s(melt_av) +
  s(elev_av) +
  s(swe_av) +
  s(base_med) +
  smp +
  s(lat, lon, bs = 'sos', k = 25)

# Build and test models with 10 fold cross-validation
for (i in 1:10) {
  index <- peak_data_dt$cv == i

  gam_huc4 <- remap::remap(
    peak_data_dt[!index, ], ws, region_id = huc4,
    model_function = gam_limit,
    buffer = 50, min_n = 20,
    distances = huc4_dist[!index, ],
    fml = gam_huc4_fml
  )

  gam_huc4_preds[index] <- predict(
    gam_huc4, peak_data_dt[index, ],
    smooth = 10,
    distances = huc4_dist[index, ]
  )
}
# 10-fold cv mse = 2.11 -- not overfitting
gam_huc4_mse <- mean((log(peak_data_dt$mult) - gam_huc4_preds)^2, na.rm = TRUE)
gam_huc4_mae <- median(abs(log(peak_data_dt$mult) - gam_huc4_preds), na.rm = TRUE)

gam_huc4 <- remap::remap(
  peak_data_dt, ws, region_id = huc4,
  model_function = gam_limit,
  buffer = 50, min_n = 20,
  distances = huc4_dist,
  fml = gam_huc4_fml
)
preds <- predict(gam_huc4, peak_data_dt, smooth = 10)
# mse = 1.99 - resub error
mse <- mean((log(peak_data_dt$mult) - preds)^2, na.rm = TRUE)
mae <- median(abs(log(peak_data_dt$mult) - preds), na.rm = TRUE)

# code to access residuals after running model
gam_huc4$models$`1019`$g_model$residuals

## run cv together for all models --------------------------------------
global_mean_preds <- rep(as.numeric(NA), nrow(peak_data_dt))
gam_nr_preds <- rep(as.numeric(NA), nrow(peak_data_dt))
gam_huc4_preds <- rep(as.numeric(NA), nrow(peak_data_dt))

for (i in 1:10) {
  index <- peak_data_dt$cv == i

  global_mean <- mean(log(peak_data_dt$mult[!index]), na.rm = TRUE)
  global_mean_preds[index] <- global_mean

  gam_nr <- mgcv::gam(log(mult) ~
                        s(temp_degc_av) +
                        s(temp_degc_med) +
                        s(snow_dep_av) +
                        s(prec_av) +
                        s(prec_max) +
                        s(prec_med) +
                        s(melt_av) +
                        s(elev_av) +
                        s(swe_av) +
                        s(base_med) +
                        smp + # 42.5
                        s(lat, lon, bs = 'sos', k = 25)
                      ,
                      data = peak_data_dt[!index, ])

  gam_nr_preds[index] <- predict(
    gam_nr, peak_data_dt[index, ]
  )

  gam_huc4 <- remap::remap(
    peak_data_dt[!index, ], ws, region_id = huc4,
    model_function = gam_limit,
    buffer = 50, min_n = 20,
    distances = huc4_dist[!index, ],
    fml = gam_huc4_fml
  )

  gam_huc4_preds[index] <- predict(
    gam_huc4, peak_data_dt[index, ],
    smooth = 10,
    distances = huc4_dist[index, ]
  )
}

mean((log(peak_data_dt$mult[-reg]) - global_mean_preds[-reg])^2, na.rm = TRUE)
median(abs(log(peak_data_dt$mult[-reg]) - global_mean_preds[-reg]), na.rm = TRUE)

gam_nr_capped <- pmax(gam_nr_preds, 0)
mean((log(peak_data_dt$mult) - gam_nr_capped)^2, na.rm = TRUE)
median(abs(log(peak_data_dt$mult) - gam_nr_capped), na.rm = TRUE)

mean((log(peak_data_dt$mult) - gam_huc4_preds)^2, na.rm = TRUE)
median(abs(log(peak_data_dt$mult) - gam_huc4_preds), na.rm = TRUE)

## plot regions and ros vs non-ros peaks -------------------------------
states <- maps::map("state", plot = FALSE, fill = TRUE) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = st_crs(ws))

pnw <- states |>
  filter(ID %in% c("washington", "oregon", "idaho", "montana",
                   "wyoming", "california", "utah", "nevada",
                   "colorado", "new mexico", "arizona"))

ws_peaks <- ws |>
  filter(huc4 %in% peak_data_dt$huc4)

ggplot(ws_peaks) +
  geom_sf(alpha = 0.5) +
  geom_sf(data = pnw, fill = "NA", size = 2) +
  geom_sf(data = peak_data_dt, size = 1, aes(col = ros #,
                                             # shape = ifelse(elev_av > 7539, "high", "low"))
  )) +
  ggtitle("Modeling Data and Regions",
          "HUC2 regions are made up of smaller HUC4 regions.") +
  scale_color_manual(values = c("black", "royalblue1")) +
  theme_void()


