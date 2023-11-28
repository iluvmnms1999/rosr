library(ggplot2)
library(dplyr)
library(gridExtra)
library(data.table)


# initial nevada work -----------------------------------------------------
nv_ms <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ge1snotel/add_base_med/ms_base_AZ.RDS")
head(nv_ms)

nv_ms_mult <- nv_ms %>%
  filter(base_med != 0) %>%
  mutate(mult = peakflow / base_med)

g1 <- ggplot(filter(nv_ms_mult, ros == "ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

g2 <- ggplot(filter(nv_ms_mult, ros == "non-ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

nv_ts <- readRDS("data-raw/ros_class/huc_match/temp_split/ge1snotel/add_base_med/ts_base_AZ.RDS")
nv_ts_mult <- nv_ts %>%
  filter(base_med != 0) %>%
  mutate(mult = peakflow / base_med)

g3 <- ggplot(filter(nv_ts_mult, ros == "ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

g4 <- ggplot(filter(nv_ts_mult, ros == "non-ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

nv_t <- readRDS("data-raw/ros_class/huc_match/temp/ge1snotel/add_base_med/t_base_AZ.RDS")
nv_t_mult <- nv_t %>%
  filter(base_med != 0) %>%
  mutate(mult = peakflow / base_med)

g5 <- ggplot(filter(nv_t_mult, ros == "ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

g6 <- ggplot(filter(nv_t_mult, ros == "non-ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

nv_ss <- readRDS("data-raw/ros_class/huc_match/swe_snotel/ge1snotel/add_base_med/ss_base_AZ.RDS")
nv_ss_mult <- nv_ss %>%
  filter(base_med != 0) %>%
  mutate(mult = peakflow / base_med)

g7 <- ggplot(filter(nv_ss_mult, ros == "ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

g8 <- ggplot(filter(nv_ss_mult, ros == "non-ros"), aes(x = base_med, y = mult)) +
  geom_point() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 13))

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, nrow = 4)

summary(filter(nv_ms_mult, ros == "ros")$mult)
summary(filter(nv_ms_mult, ros == "non-ros")$mult)

summary(filter(nv_ss_mult, ros == "ros")$mult)
summary(filter(nv_ss_mult, ros == "non-ros")$mult)

summary(filter(nv_t_mult, ros == "ros")$mult)
summary(filter(nv_t_mult, ros == "non-ros")$mult)

summary(filter(nv_ts_mult, ros == "ros")$mult)
summary(filter(nv_ts_mult, ros == "non-ros")$mult)




#### melt_snotel mults ####
## melt_snotel
# combine all states into one data table
combo <- data.table()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "AZ", "WA", "UT")
for (i in seq_along(states)) {
  temp <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel/add_base_med/ms_base_",
  states[i], ".RDS"))
  combo <- rbind(combo, temp)
}

# add multiplier variable
add_mult <- combo[base_med != 0 & sign(base_med) != -1,
                  mult := peakflow / base_med,
                  by = state]

# find mean and median multipliers between all states
tot <- add_mult[, .(mean = mean(mult, na.rm = TRUE),
                    med = median(mult,na.rm = TRUE),
                    n = .N),
         by = ros]
# overall mult (using median):
tot[2, 3] / tot[1, 3] # 2.957681

# look at mean and median multipliers for each state
add_mult[, .(mean_mult = mean(mult, na.rm = TRUE),
             med_mult = median(mult, na.rm = TRUE),
             n = .N),
         by = .(state, ros)][order(ros)][order(state)]

## plots
# hexbin chart of ros vs non-ros mults faceted together
g1 <- ggplot(add_mult, aes(x = base_med, y = mult)) +
  geom_hex() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 20)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 14)) +
  scale_fill_viridis_c() +
  theme_bw() +
  ggtitle("SNOTEL - Melt") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ ros)
g1

#### temp_split mults ####
## temp_split
# combine all states into one data table
combo <- data.table()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "AZ", "WA", "UT")
for (i in seq_along(states)) {
  temp <- readRDS(paste0("data-raw/ros_class/huc_match/temp_split/ge1snotel/add_base_med/ts_base_",
                         states[i], ".RDS"))
  combo <- rbind(combo, temp)
}

# add multiplier variable
add_mult <- combo[base_med != 0 & sign(base_med) != -1,
                  mult := peakflow / base_med,
                  by = state]

# find mean and median multipliers between all states
tot <- add_mult[, .(mean = mean(mult, na.rm = TRUE),
                    med = median(mult,na.rm = TRUE)),
                by = ros]
# overall mult (using median):
tot[2, 3] / tot[1, 3] # 2.46028

## plots
g2 <- ggplot(add_mult, aes(x = base_med, y = mult)) +
  geom_hex() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 20)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 14)) +
  scale_fill_viridis_c() +
  theme_bw() +
  ggtitle("SNOTEL - Split Temp") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ ros)
g2
#### temp_mults ####
## melt_snotel
# combine all states into one data table
combo <- data.table()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "AZ", "WA", "UT")
for (i in seq_along(states)) {
  temp <- readRDS(paste0("data-raw/ros_class/huc_match/temp/ge1snotel/add_base_med/t_base_",
                         states[i], ".RDS"))
  combo <- rbind(combo, temp)
}

# add multiplier variable
add_mult <- combo[base_med != 0 & sign(base_med) != -1,
                  mult := peakflow / base_med,
                  by = state]

# find mean and median multipliers between all states
tot <- add_mult[, .(mean = mean(mult, na.rm = TRUE),
                    med = median(mult,na.rm = TRUE)),
                by = ros]
# overall mult (using median):
tot[2, 3] / tot[1, 3] # 2.480094

g3 <- ggplot(add_mult, aes(x = base_med, y = mult)) +
  geom_hex() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 20)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 14)) +
  scale_fill_viridis_c() +
  theme_bw() +
  ggtitle("SNOTEL - Temp") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ ros)
g3

#### swe_snotel mults ####
## swe_snotel
# combine all states into one data table
combo <- data.table()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "AZ", "WA", "UT")
for (i in seq_along(states)) {
  temp <- readRDS(paste0("data-raw/ros_class/huc_match/swe_snotel/ge1snotel/add_base_med/ss_base_",
                         states[i], ".RDS"))
  combo <- rbind(combo, temp)
}

# add multiplier variable
add_mult <- combo[base_med != 0 & sign(base_med) != -1,
                  mult := peakflow / base_med,
                  by = state]

# find mean and median multipliers between all states
tot <- add_mult[, .(mean = mean(mult, na.rm = TRUE),
                    med = median(mult,na.rm = TRUE)),
                by = ros]
# overall mult (using median):
tot[2, 3] / tot[1, 3] # 2.929367

g4 <- ggplot(add_mult, aes(x = base_med, y = mult)) +
  geom_hex() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 20)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 14)) +
  scale_fill_viridis_c() +
  theme_bw() +
  ggtitle("SNOTEL - SWE") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ ros)
g4

# combined plots ----------------------------------------------------------
gridExtra::grid.arrange(g1, g2, g3, g4)

x <- readRDS("data-raw/snotel/snotel_clean_OR.RDS")

