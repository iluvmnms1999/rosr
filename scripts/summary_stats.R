library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

peaks <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")

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
  geom_path(lwd = 0.9) +
  geom_point() +
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

## PAPER
# looking at distributions of important variables (temp, swe, precip, and now add surge)
png("figures/ch2/paper/ros_sums_all.png", width = 7, height = 6, units = "in", res = 200)
g1 <- peaks |>
  ggplot() +
  stat_density(aes(x = temp_degc_av, color = ros),
               geom = "line", position = "identity", lwd = 1) +
  stat_density(aes(x = temp_degc_av, color = "gray60"), linetype = "dashed", geom = "line",
               position = "identity", inherit.aes = FALSE) +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d"),
                     labels = c("Overall", "Non-ROS", "ROS")) +
  guides(color = guide_legend(override.aes = list(lty = c("dashed", "solid", "solid"),
                                                  lwd = c(0.5, 1, 1)))) +
  xlab("Temperature (in \u00B0C)") +
  ylab("") +
  scale_x_continuous(limits = c(-10, 20), breaks = seq(-10, 20, 5)) +
  scale_y_continuous(limits = c(0, 0.2)) +
  theme_bw() +
  theme(legend.position = c(0.835, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.key.height = unit(0.5, "cm"),
        panel.grid.minor = element_blank())

g2 <- peaks |>
  ggplot() +
  stat_density(aes(x = prec_max, color = ros),
               geom = "line", position = "identity", lwd = 1, show.legend = FALSE) +
  stat_density(aes(x = prec_max, color = "gray60"), linetype = "dashed", geom = "line",
               position = "identity", inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d")) +
  xlab("Precipitation (in mm, log2-scale)") +
  ylab("Density") +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(1, 2 ^ 9)) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), limits = c(0, 0.4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

g3 <- peaks |>
  ggplot() +
  stat_density(aes(x = swe_av, color = ros),
               geom = "line", position = "identity", lwd = 1, show.legend = FALSE) +
  stat_density(aes(x = swe_av, color = "gray60"), linetype = "dashed", geom = "line",
               position = "identity", inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d")) +
  xlab("SWE (in mm, log2-scale)") +
  ylab("") +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 6),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3)) +
  # scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) +
  # scale_y_continuous(limits = c(0, 0.008)) +
  # ggtitle("Proportion of Peaks per Month by ROS") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

g4 <- peaks %>%
  ggplot() +
  # geom_density(aes(x = mult, color = ros), linewidth = 1.3, show.legend = FALSE) +
  stat_density(aes(x = mult, color = ros),
               geom = "line", position = "identity", lwd = 1, show.legend = FALSE) +
  stat_density(aes(x = mult, color = "gray60"), linetype = "dashed", geom = "line",
               position = "identity", inherit.aes = FALSE, show.legend = FALSE) +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 6),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  xlab("Surge (cfs, log2-scale)") +
  ylab("Density") +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d")) +
  # guides(color = guide_legend(override.aes = list(lty = c("dashed", "solid", "solid"),
  #                                                 lwd = c(0.5, 1, 1)))) +
  # labs(color = "ROS Class") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

grid.arrange(g4, g1, g2, g3, nrow = 2)
dev.off()

# some numeric exploration
peaks |>
  group_by(ros) |>
  summarize(med = median(temp_degc_av, na.rm = TRUE))


## PAPER
## looking at distribution of empirical peakflow
emp_surge <- peaks |>
  # sf::st_drop_geometry() |>
  group_by(id, ros) |>
  summarize(med_surge = median(peakflow),
            count = n()) |>
  pivot_wider(names_from = "ros", values_from = c("med_surge", "count")) |>
  filter(`count_non-ros` >= 2, count_ros >= 2) |>
  mutate(emp_rat = med_surge_ros / `med_surge_non-ros`)

# density plot
png("figures/ch2/paper/emp_peakflow.png", height = 5, width = 6, units = "in", res = 300)
emp_surge |>
  ggplot(aes(x = emp_rat)) +
  geom_density(bw = 0.09) + # smooth it a bit
  # geom_vline(aes(xintercept = median(emp_rat)), col = "red") +
  # annotate("text", x = 1.45, y = 0, label = "median = 1.164", color = "red", size = 4) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  # scale_y_continuous(limits = c(0, 1)) +
  geom_vline(aes(xintercept = median(emp_rat)), col = "gray45") +
  annotate("text", x = 0.78, y = 0, label = "median = 1.013", color = "gray45", size = 3.5) +
  geom_vline(aes(xintercept = mean(emp_rat)), col = "gray45", lty = "dashed") +
  annotate("text", x = 1.27, y = 0, label = "mean = 1.052", color = "gray45", size = 3.5) +
  # ggtitle("Distribution of Empirical Stream Surge Ratios") +
  xlab("Ratio") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))
dev.off()

summary(emp_surge$emp_rat)

## PAPER
## looking at distribution of empirical surge ratio
emp_surge <- peaks |>
  # sf::st_drop_geometry() |>
  group_by(id, ros) |>
  summarize(med_surge = median(mult),
            count = n()) |>
  pivot_wider(names_from = "ros", values_from = c("med_surge", "count")) |>
  filter(`count_non-ros` >= 2, count_ros >= 2) |>
  mutate(emp_rat = med_surge_ros / `med_surge_non-ros`)

# density plot
png("figures/ch2/paper/emp_ratios.png", height = 5, width = 6, units = "in", res = 300)
emp_surge |>
  ggplot(aes(x = emp_rat)) +
  geom_density(bw = 0.09) + # smooth it a bit
  # geom_vline(aes(xintercept = median(emp_rat)), col = "red") +
  # annotate("text", x = 1.45, y = 0, label = "median = 1.164", color = "red", size = 4) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_vline(aes(xintercept = median(emp_rat)), col = "gray45") +
  annotate("text", x = 0.75, y = 0, label = "median = 1.002", color = "gray45", size = 3.5) +
  geom_vline(aes(xintercept = mean(emp_rat)), col = "gray45", lty = "dashed") +
  annotate("text", x = 1.65, y = 0, label = "mean = 1.431", color = "gray45", size = 3.5) +
  # ggtitle("Distribution of Empirical Stream Surge Ratios") +
  xlab("Ratio") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))
dev.off()

summary(emp_surge$emp_rat)

 ## PAPER
# boxplot
png("figures/ch2/paper/emp_rat_box.png", height = 2, width = 5, units = "in", res = 300)
emp_surge |>
  ggplot(aes(x = emp_rat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 18) +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 8),
                     limits = c(0.01, 2 ^ 6),
                     labels = scales::number_format(accuracy = 0.01)
                     ) +
  scale_y_continuous(limits = c(-.4, .4)) +
  xlab("Ratio (log2-scale)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
dev.off()
