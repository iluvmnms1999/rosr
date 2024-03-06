library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)

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



# visual distr of each important variable ---------------------------------

# facet following plot when ros_num == 1 and ros_num == 0
peak_data_dt %>% # use median of overall distrs from this plot (ros vs non-ros)
  ggplot(aes(x = temp_degc_av, col = ros)) +
  geom_density()

peak_data_dt %>% # 0 for non-ros, med ann max for ros
  ggplot(aes(x = snow_dep_av, col = ros)) +
  geom_density()

peak_data_dt %>% # 25th percentile of annual max for non-ros, 75th for ros
  ggplot(aes(x = log(prec_max), col = ros)) +
  geom_density()

peak_data_dt %>% # 0 for non-ros, med ann max for ros
  ggplot(aes(x = swe_av, col = ros)) +
  geom_density()

peak_data_dt %>% # median overall by location
  ggplot(aes(x = log(base_med), col = ros)) +
  geom_density()

# predict with gam twice for each location using both profiles, compute ratio
# of both predictions

# you're gonna need go back to the original data and get the summary stats
# for each station


# run gam on each gage ----------------------------------------------------
# iterate through each gage with peaks
set.seed(42) #Set seed for reproducibility

for (i in seq_along(unique(peak_data_dt$id))) {
  temp <- peak_data_dt |>
    filter(id == unique(peak_data_dt$id)[i])
  ros <- temp |>
    filter(ros_num == 1)
  non_ros <- temp |>
    filter(ros_num == 0)

  ros_preds <- rep(as.numeric(NA), nrow(ros))
  for (i in 1:5) {
    ros$weight <- sample(c(0,1), size = nrow(ros),
                                  replace = TRUE,
                                  prob = c(0.2, 0.8))
    index <- ros$weights == 0

    gam_nr <- mgcv::gam(log(mult) ~
                          s(temp_degc_av) +
                          s(snow_dep_av) +
                          s(prec_max) +
                          s(swe_av) +
                          s(base_med) +
                          s(lat, lon, bs = 'sos', k = 25),
                        data = ros,
                        weights = ros$weight)

    ros_preds[index] <- predict(
      gam_nr, ros[ros$weight == 0,]
    )
  }
  gam_nr_capped <- pmax(gam_nr_preds, 0)

}



