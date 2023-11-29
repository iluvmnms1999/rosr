
# regression tree ---------------------------------------------------------
# example of fitting regression tree in R:
# https://www.statology.org/classification-and-regression-trees-in-r/

# predictor variables: daily temp, daily snow depth, daily prec,
# daily soil mp; elevation, soil type
# Qs:
#   How do we want to aggregate the station data?
#     - treat every single peak as an independent observation
#   What method should we use to summarize all daily variables between stations?
#   We talked about using these daily and time-invariant measurements to
#     predict an hourly process - if we're trying to predict the multiplier
#     produced by the ratio of peakflow to baseflow, that's not reported on
#     an hourly level - only reported whenever there's a peak... is that a
#     problem?
#   If we're aggregating the predictors, what kind of aggregated response are
#     we trying to predict?

# What was temp, precip, flow, etc for each peaks?
# Make big matrix with all observations representing a single peak (include
#   all variables)

library(rpart)
library(rpart.plot)

## make data matrix
# get peaks data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
ros_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel",
                      "/add_base_med/ms_base_", states[i], ".RDS"))
  ros_all <- rbind(ros_all, x)
}
# get just date instead of datetime
ros_all[, date := as.Date(dt)]

# add environmental data
snotel_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                      states[i], ".RDS"))
  snotel_all <- rbind(snotel_all, x)
}
# average measurements between stations in huc region
snotel_av <- snotel_all[, .(elev_av = mean(elev, na.rm = TRUE),
               temp_degc_av = mean(temp_degc, na.rm = TRUE),
               prec_av = mean(prec, na.rm = TRUE),
               snow_dep_av = mean(snow_dep, na.rm = TRUE),
               swe_av = mean(swe, na.rm = TRUE),
               soil_mp8in_av = mean(soil_mp8in, na.rm = TRUE),
               soil_mp20in_av = mean(soil_mp20in),
               melt_av = mean(melt, na.rm = TRUE)
              ),
              by = .(date, huc)]

# join snotel and ros_class by huc and date
ros_all[snotel_av, on = .(huc, date)]

