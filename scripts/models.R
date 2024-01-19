
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
library(data.table)


## make data matrix --------------------------------------------------------
# get peaks data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
ros_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel",
                      "/add_base_med_ref/ms_baseref_", states[i], ".RDS"))
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
# average and max/min measurements between stations in huc regions
snotel_av <- snotel_all[, .(elev_av = mean(elev),
                            elev_min = min(elev),
                            elev_max = max(elev),
                            temp_degc_av = mean(temp_degc),
                            temp_degc_min = min(temp_degc),
                            temp_degc_max = max(temp_degc),
                            prec_av = mean(prec),
                            prec_min = min(prec),
                            prec_max = max(prec),
                            snow_dep_av = mean(snow_dep),
                            snow_dep_min = min(snow_dep),
                            snow_dep_max = max(snow_dep),
                            swe_av = mean(swe),
                            swe_min = min(swe),
                            swe_max = max(swe),
                            soil_mp8in_av = mean(soil_mp8in),
                            soil_mp8in_min = min(soil_mp8in),
                            soil_mp8in_max = max(soil_mp8in),
                            soil_mp20in_av = mean(soil_mp20in),
                            soil_mp20in_min = min(soil_mp20in),
                            soil_mp20in_max = max(soil_mp20in),
                            melt_av = mean(melt),
                            melt_min = min(melt),
                            melt_max = max(melt)),
                        by = .(date, huc)]

# join snotel and ros_class by huc and date
peak_data_dt <- snotel_av[ros_all, on = .(huc, date)]

# add multiplier
peak_data_dt <- peak_data_dt[base_med > 0][, mult := peakflow / base_med]

# saveRDS(peak_data_dt, "data-raw/modeling/peak_data_dt.rds")



## fit regression tree -----------------------------------------------------
# peak_data_dt <- readRDS("data-raw/modeling/peak_data_dt.rds")

# build the initial tree
tree <- rpart(mult ~ temp_degc_av + temp_degc_min + temp_degc_max +
                snow_dep_av + snow_dep_min + snow_dep_max + prec_av + prec_min +
                prec_max + soil_mp8in_av + soil_mp8in_min + soil_mp8in_max +
                soil_mp20in_av + soil_mp20in_min + soil_mp20in_max + melt_av +
                melt_min + melt_max + elev_av + elev_min + elev_max + swe_av +
                swe_min + swe_max + ros,
              data = peak_data_dt, control = rpart.control(cp = .001))

# view results
printcp(tree)
rpart.plot(tree)

## prune tree
# identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# produce pruned tree based on the best cp value
pruned_tree <- prune(tree, cp = best)

# plot the pruned tree
prp(pruned_tree,
    faclen = 0, # use full names for factor labels
    extra = 1, # display number of obs. for each terminal node
    roundint = F, # don't round to integers in output
    digits = 5) # display 5 decimal places in output

## fit gam ---------------------------------------------------------------------
# mgcv example:
# https://m-clark.github.io/generalized-additive-models/application.html
library(mgcv)

peak_data_dt <- peak_data_dt[, ros_num := ifelse(ros == "ros", 1, 0)]

# just ROS
# first try linear model
mod_lm = gam(mult ~ ros, data = peak_data_dt)
summary(mod_lm)

# now try gam
mod_gam1 = gam(mult ~ s(temp_degc_av, bs = "tp"), data = peak_data_dt)
summary(mod_gam1)
plot(mod_gam1)

# more features
mod_lm <- gam(mult ~ s(temp_degc_av) + s(temp_degc_min) + s(temp_degc_max) +
  s(snow_dep_av) + s(snow_dep_min) + s(snow_dep_max) + s(prec_av) + s(prec_min) +
  s(prec_max) + s(soil_mp8in_av) + s(soil_mp8in_min) + s(soil_mp8in_max) +
  s(soil_mp20in_av) + s(soil_mp20in_min) + s(soil_mp20in_max) + s(melt_av) +
  s(melt_min) + s(melt_max) + s(elev_av) + s(elev_min) + s(elev_max) + s(swe_av) +
  s(swe_min) + s(swe_max) + ros_num, data = peak_data_dt)
summary(mod_lm)

names(peak_data_dt)
pairs(peak_data_dt[, c(3:26, 30:31, 33)])

# remotes::install_github("mlverse/chattr")

