
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


