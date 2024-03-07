library(data.table)
library(tidyverse)

# av temp: use median of overall distrs from the plot (ros vs non-ros)
# av snow dep: 0 for non-ros, med ann max for ros
# prec_max (or log): 25th percentile of annual max for non-ros, 75th for ros
# av swe: 0 for non-ros, med ann max for ros
# base_med (log): median overall by location

# predict with gam twice for each location using both profiles, compute ratio
# of both predictions and examine distribution of all ratios

# import peak data
peak_data <- readRDS("data-raw/modeling/peak_data_sf.rds")

# get snotel summaries
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
snotel_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                      states[i], ".RDS"))
  snotel_all <- rbind(snotel_all, x)
}

snotel_summaries <- data.frame()
for (i in seq_along(unique(snotel_all$huc))) {
  temp <- snotel_all[huc == unique(snotel_all$huc)[i]]
  temp[, year := data.table::year(date)]

  snowdep <- temp[, .(ann_max = max(snow_dep, na.rm = TRUE)), by = year]
  med_snowdep <- median(snowdep$ann_max, na.rm = TRUE)

  prec_max <- temp[, .(ann_max = max(prec, na.rm = TRUE)), by = year]
  prec_25 <- quantile(prec_max$ann_max, .25, na.rm = TRUE, names = FALSE)
  prec_75 <- quantile(prec_max$ann_max, .75, na.rm = TRUE, names = FALSE)

  swe <- temp[, .(ann_max = max(swe, na.rm = TRUE)), by = year]
  med_swe <- median(swe$ann_max, na.rm = TRUE)

  vec <- c(unique(snotel_all$huc)[i], med_snowdep, prec_25, prec_75, med_swe)

  snotel_summaries <- rbind(snotel_summaries, vec)
}
colnames(snotel_summaries) <- c("huc", "med_snowdep", "prec_25", "prec_75", "med_swe")
data.table::setDT(snotel_summaries)

snot_sum <- snotel_summaries[huc %in% unique(peak_data$huc)]
saveRDS(snot_sum, "data-raw/modeling/snot_sum.rds")

# baseflow summaries by streamgage
data.table::setDT(peak_data)
med_bf <- peak_data |>
  group_by(id) |>
  summarise(med_bf = median(base_med),
            huc = huc) |>
  slice(1) |>
  as.data.frame()

var_summaries <- left_join(med_bf, snotel_summaries, join_by(huc))
saveRDS(var_summaries, "data-raw/modeling/var_summaries.rds")
