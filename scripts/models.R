
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


library(data.table)
library(imputeTS)

## make data matrix --------------------------------------------------------
# get peaks data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
peaks_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel",
                      "/add_base_med_ref/ms_baseref_", states[i], ".RDS"))
  peaks_all <- rbind(peaks_all, x)
}
# get just date instead of datetime
peaks_all[, date := as.Date(dt)]

# add environmental data
snotel_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                      states[i], ".RDS"))
  snotel_all <- rbind(snotel_all, x)
}

# interpolate NAs
# apply(snotel_all, 2, function(x) sum(is.na(x)))
# missing values summary: 8% temp_degc, 2% prec, 44% snow_dep, 2% swe,
#  77% soil_mp8in, 78% soil_mp20in, 3% melt
snotel_all[, ":="(temp_degc = ifelse(sum(is.na(temp_degc)) < length(temp_degc),
                                     na_interpolation(temp_degc, "linear"),
                                     temp_degc),
                   prec = ifelse(sum(is.na(prec)) < length(prec),
                                 na_interpolation(prec, "linear"),
                                 prec),
                   snow_dep = ifelse(sum(is.na(snow_dep)) < length(snow_dep),
                                     na_interpolation(snow_dep, "linear"),
                                     snow_dep),
                   swe = ifelse(sum(is.na(swe)) < length(swe),
                                na_interpolation(swe, "linear"),
                                swe),
                   soil_mp8in = ifelse(sum(is.na(soil_mp8in)) < length(soil_mp8in),
                                       na_interpolation(soil_mp8in, "linear"),
                                       soil_mp8in),
                   soil_mp20in = ifelse(sum(is.na(soil_mp20in)) < length(soil_mp20in),
                                        na_interpolation(soil_mp20in, "linear"),
                                        soil_mp20in),
                   melt = ifelse(sum(is.na(melt)) < length(melt),
                                 na_interpolation(melt, "linear"),
                                 melt)), by = id]


# completed imputed data table stored as follows (not by id):
# saveRDS(peak_data_dt, "data-raw/modeling/peak_data_dt_imputed.rds")


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
peak_data_dt <- snotel_av[peaks_all, on = .(huc, date)]

# add multiplier
peak_data_dt <- peak_data_dt[base_med > 0][, mult := peakflow / base_med]

# saveRDS(peak_data_dt, "data-raw/modeling/peak_data_dt.rds")


## fit regression tree -----------------------------------------------------
library(rpart)
library(rpart.plot)
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
# library(gam)
library(dplyr)

peak_data_dt <- peak_data_dt[, ros_num := ifelse(ros == "ros", 1, 0)]

# just ROS
# first try ros with interactions
mod_lm <-  mgcv::gam(log(mult) ~ ros_num + temp_degc_av + prec_av +
                       soil_mp8in_av + elev_av + swe_av + melt_av
                     , data = peak_data_dt)
summary(mod_lm)

mod_lm <-  mgcv::gam(log(mult) ~ ros_num + ros_num * temp_degc_av +
                       ros_num * prec_av + ros_num * soil_mp8in_av +
                       ros_num * elev_av + ros_num * swe_av + ros_num * melt_av
                     , data = peak_data_dt)
summary(mod_lm)

# now try all variables without smoothing
mod_gam1 <- mgcv::gam(log(mult) ~ temp_degc_av + temp_degc_min + temp_degc_max +
                        snow_dep_av + snow_dep_min + snow_dep_max + prec_av +
                        prec_min + prec_max + soil_mp8in_av + soil_mp8in_min +
                        soil_mp8in_max + soil_mp20in_av + soil_mp20in_min +
                        soil_mp20in_max + melt_av + melt_min + melt_max + elev_av +
                        elev_min + elev_max + swe_av + swe_min + swe_max + ros_num,
                      data = peak_data_dt)
summary(mod_gam1)

# with smoothing
mod_gam2 <- mgcv::gam(log(mult) ~
                        # s(temp_degc_av) + s(temp_degc_min) + s(temp_degc_max) +
                        s(snow_dep_av) + s(snow_dep_min) + s(snow_dep_max) +
                        s(prec_av) + s(prec_min) + s(prec_max) +
                        s(soil_mp8in_av) + s(soil_mp8in_min) + s(soil_mp8in_max) +
                        # s(soil_mp20in_av) + s(soil_mp20in_min) + s(soil_mp20in_max) +
                        s(melt_av) + s(melt_min) + s(melt_max) +
                        s(elev_av) + s(elev_min) + s(elev_max) +
                        s(swe_av) + s(swe_min) + s(swe_max) + ros_num,
                      data = peak_data_dt[ros == "non-ros"])
summary(mod_gam2)

# splitting train/test data w/ k-fold cross validation
# EX: https://stackoverflow.com/questions/48546445/how-to-predict-test-data-using-a-gam-with-mrf-smooth-and-neighborhood-structure
# og smoothing specification: crime ~ s(disctrict, k = nrow(data), bs = 'tp', xt = list(nb = nb))
# Apply k-fold cross validation
mses <- data.frame() # Create empty df to store CV squared error values
scores <- data.frame() # Create empty df to store CV R2 values
dev_exp <- data.frame() # Create empty df to store CV deviance explained values
set.seed(42) #Set seed for reproducibility
k <- 5 #Define number of folds
for (i in 1:k) {
  # Create weighting column
  peak_data_dt$weight <- sample(c(0,1), size = nrow(peak_data_dt),
                                replace = TRUE,
                                prob = c(0.2, 0.8)) # 0 Indicates testing sample,
  # 1 training sample
  # Run GAM with smoothing
  # ctrl <- gam.control(nthreads = 6) # Set controls
  m <- mgcv::gam(log(mult) ~ s(temp_degc_av) + s(snow_dep_av) + s(prec_av) +
                   s(melt_av) + s(elev_av) + s(swe_av) + ros_num
                 + ros_num * temp_degc_av +
                   ros_num * prec_av + ros_num * soil_mp8in_av +
                   ros_num * elev_av + ros_num * swe_av + ros_num * melt_av,
                 data = peak_data_dt,
                 weights = peak_data_dt$weight, # Use only weight==1 observations (training)
                 # method = "REML",
                 # control = ctrl,
                 # family = scat(),
                 # gamma = 1.4
  )
  # Generate test dataset
  testdata <- peak_data_dt[peak_data_dt$weight == 0,] #Select test data by weight
  # Predict test data -- TOO MANY NA'S -- leave out soil mp to retain more but accuracy goes down
  pred <- predict(m, newdata = testdata, type = "response")
  # Extract MSES
  mses[i,1] <- mean((log(peak_data_dt$mult[peak_data_dt$weight == 0]) - pred) ^ 2, na.rm = TRUE)
  scores[i,1] <- summary(m)$r.sq
  dev_exp[i,1] <- summary(m)$dev.expl
}
av.mse.GMRF <- mean(mses$V1)
av.r2.GMRF <- mean(scores$V1)
av.dev_exp.GMRF <- mean(dev_exp$V1)


## random forests ----------------------------------------------------------

# random forest example:
# https://www.statology.org/random-forest-in-r/
# tuning:
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
library(randomForest)
library(mlbench)
library(caret)
library(randomForestVIP)
# devtools::install_github("KelvynBladen/randomForestVIP")
# consider using ranger or kelvyn's package

peak_data_imp <- readRDS("data-raw/modeling/peak_data_dt_imputed.rds")
peak_data_imp <- peak_data_imp[, ros_num := ifelse(ros == "ros", 1, 0)]
peak_data_imp <- as.data.frame(peak_data_imp)
peak_data_imp <- peak_data_imp[!is.na(peak_data_imp$elev_av), ]

# replace NAs with column medians
# for(i in 1:ncol(peak_data_dt)) {
#   peak_data_dt[, i][is.na(peak_data_dt[, i])] <- median(peak_data_dt[, i], na.rm=TRUE)
# }
x <- peak_data_imp[, c(3:26, 34)] # define predictor variables
y <- peak_data_imp$mult # define response variable

## WITH CARET
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(x, y, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


## WITH randomForest
# fit random forest
set.seed(1)

rf <- randomForest(log(mult) ~
                     # temp_degc_av + temp_degc_min + temp_degc_max +
                     snow_dep_av + snow_dep_min + snow_dep_max +
                     prec_av + prec_min + prec_max +
                     soil_mp8in_av + soil_mp8in_min + soil_mp8in_max +
                     # soil_mp20in_av + soil_mp20in_min + soil_mp20in_max +
                     melt_av + melt_min + melt_max +
                     elev_av + elev_min + elev_max +
                     swe_av + swe_min + swe_max,
                   data = peak_data_imp, ntree = 500, mtry = 3, importance = TRUE)

rf
which.min(rf$mse)
sqrt(rf$mse[which.min(rf$mse)])
plot(rf)
varImpPlot(rf)

rf_tuned <- tuneRF(
  x = peak_data_dt[, c(3:26, 34)], # define predictor variables
  y = peak_data_dt$mult, # define response variable
  ntreeTry = 500,
  mtryStart = 4,
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE, # don't show real-time progress
  plot = TRUE
)

varImpPlot(rf_tuned, sort = TRUE, n.var = 10)
class(rf_tuned)




kelvyn <- mtry_compare(log(mult) ~
                     # temp_degc_av + temp_degc_min + temp_degc_max +
                     snow_dep_av + snow_dep_min + snow_dep_max +
                     prec_av + prec_min + prec_max +
                     soil_mp8in_av + soil_mp8in_min + soil_mp8in_max +
                     # soil_mp20in_av + soil_mp20in_min + soil_mp20in_max +
                     melt_av + melt_min + melt_max +
                     elev_av + elev_min + elev_max +
                     swe_av + swe_min + swe_max,
                   data = peak_data_imp)

kelvyn$gg_var_imp_permute
