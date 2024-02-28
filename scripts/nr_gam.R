library(mgcv)
library(dplyr)

# load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf.rds")

form_lst <- list(
  log(mult) ~
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
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_av) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_av) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_max) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_max) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_sum) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_sum) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_med) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_med) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(base_med) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25)
)

# 10 fold cross-validation for non-regionalized gam list
gam_nr_mse <- rep(as.numeric(NA), length(form_lst))
gam_nr_mae <- rep(as.numeric(NA), length(form_lst))
for (j in seq_along(form_lst)) {
  gam_nr_preds <- rep(as.numeric(NA), nrow(peak_data_dt))
  for (i in 1:10) {
    index <- peak_data_dt$cv == i

    gam_nr <- mgcv::gam(form_lst[[j]], data = peak_data_dt[!index, ])

    gam_nr_preds[index] <- predict(
      gam_nr, peak_data_dt[index, ]
    )
  }
  gam_nr_capped <- pmax(gam_nr_preds, 0)
  gam_nr_mse[j] <- mean((log(peak_data_dt$mult) - gam_nr_capped)^2, na.rm = TRUE)
  gam_nr_mae[j] <- median(abs(log(peak_data_dt$mult) - gam_nr_capped), na.rm = TRUE)
}

# lowest mse overall (1.622):
# log(mult) ~
#   s(temp_degc_av) +
#   s(snow_dep_av) +
#   s(prec_max) +
#   s(swe_av) +
#   s(base_med) +
#   s(lat, lon, bs = 'sos', k = 25)

