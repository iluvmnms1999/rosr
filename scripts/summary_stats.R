library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

peaks <- readRDS("data-raw/modeling/peak_data_sf.rds")

## PRESENTATION
# compare baseflow to surge for ros vs non-ros
png("figures/ch2/presentation/ss_hexbin_melt.png", width = 6, height = 4, units = "in", res = 250)
ggplot(peaks, aes(x = base_med, y = mult)) +
  geom_hex() +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 20)) +
  scale_y_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 14)) +
  scale_fill_viridis_c() +
  theme_bw() +
  ggtitle("Hexbin Plot of Baseflow vs. Surge") +
  xlab("Baseflow (log2)") +
  ylab("Surge, g (log2)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ ros)
dev.off()

# in which month do peaks usually occur?
month_counts <- peaks |>
  mutate(month = month(dt)) |>
  group_by(ros, month) |>
  summarise(count = n()) |>
  group_by(ros) |>
  mutate(count_prop = count / sum(count))

## PRESENTATION
png("figures/ch2/presentation/ss_month_props.png", width = 5, height = 4, units = "in", res = 300)
ggplot(month_counts, aes(x = as.factor(month), y = count_prop)) +
  geom_col(fill = "#023E8A") +
  xlab("Month") +
  ylab("Proportion of Peaks") +
  ggtitle("Proportion of Peaks per Month by ROS") +
  theme_bw() +
  facet_wrap(~ ros, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

## PAPER
png("figures/ch2/paper/ss_month_props_paper.png", width = 5, height = 4, units = "in", res = 300)
ggplot(month_counts, aes(x = as.factor(month), y = count_prop, group = ros,
                         color = ros)) +
  geom_path(lwd = 1) +
  xlab("Month") +
  ylab("Proportion of Peaks") +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_color_manual(values = c("#bf812d", "#41ab5d"),
                     labels = c("Non-ROS", "ROS")) +
  theme_bw() +
  theme(legend.position = c(0.83, 0.88),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key = element_blank())
dev.off()

# looking at distributions of important variables from GAM
png("figures/ch2/mean_temp_comp.png", width = 5, height = 4, units = "in", res = 200)
ggplot(peaks, aes(x = temp_degc_av)) +
  geom_density() +
  facet_wrap(~ ros) # +
  # xlab("Month") +
  # ylab("Proportion of Peaks") +
  # ggtitle("Proportion of Peaks per Month by ROS") +
  # theme_bw() +
  # theme(plot.title = element_text(hjust = 0.5))
dev.off()

png("figures/ch2/mean_snowdep_comp.png", width = 5, height = 4, units = "in", res = 200)
ggplot(peaks, aes(x = snow_dep_av)) +
  geom_density() +
  facet_wrap(~ ros) # +
  # xlab("Month") +
  # ylab("Proportion of Peaks") +
  # ggtitle("Proportion of Peaks per Month by ROS") +
  # theme_bw() +
  # theme(plot.title = element_text(hjust = 0.5))
dev.off()

png("figures/ch2/max_prec_comp.png", width = 5, height = 4, units = "in", res = 200)
ggplot(peaks, aes(x = prec_max)) +
  geom_density() +
  facet_wrap(~ ros) # +
# xlab("Month") +
# ylab("Proportion of Peaks") +
# ggtitle("Proportion of Peaks per Month by ROS") +
# theme_bw() +
# theme(plot.title = element_text(hjust = 0.5))
dev.off()

png("figures/ch2/mean_swe_comp.png", width = 5, height = 4, units = "in", res = 200)
ggplot(peaks, aes(x = swe_av)) +
  geom_density() +
  facet_wrap(~ ros) # +
# xlab("Month") +
# ylab("Proportion of Peaks") +
# ggtitle("Proportion of Peaks per Month by ROS") +
# theme_bw() +
# theme(plot.title = element_text(hjust = 0.5))
dev.off()

png("figures/ch2/mean_baseflow_comp.png", width = 5, height = 4, units = "in", res = 200)
ggplot(peaks, aes(x = base_med)) +
  geom_density() +
  facet_wrap(~ ros) # +
# xlab("Month") +
# ylab("Proportion of Peaks") +
# ggtitle("Proportion of Peaks per Month by ROS") +
# theme_bw() +
# theme(plot.title = element_text(hjust = 0.5))
dev.off()

