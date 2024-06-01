library(ggplot2)
library(tidyverse)

# peaks data from melt method before getting rid of invalid observations
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
peaks_all <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/ros_class/huc_match/melt_snotel/ge1snotel",
                      "/add_base_med_ref/ms_baseref_", states[i], ".RDS"))
  peaks_all <- rbind(peaks_all, x)
}

# peaks data prepped for modeling
peaks <- readRDS("data-raw/modeling/peak_data_sf.rds") |>
  filter(mult > 0)

## PRESENTATION
png("figures/ch2/presentation/rosdistrcor.png", width = 5, height = 5, units = "in", res = 250)
peaks %>%
  ggplot() +
  geom_density(aes(x = mult), linetype = 2, color = "gray70", show.legend = TRUE) +
  geom_density(aes(x = mult, color = ros), linewidth = 1.3, show.legend = FALSE) +
  stat_density(aes(x = mult, color = ros),
               geom = "line", position = "identity", lwd = 1.3) +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 6),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  xlab("Surge (cfs, log2-scale)") +
  ylab("Density") +
  ggtitle("Flood Distributions of ROS vs non-ROS") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("#bf812d", "#41ab5d", "gray70"),
                     labels = c("non-ros", "ros", "overall")) +
  scale_linetype_manual(values = c(1, 1, 2)) +
  labs(color = "ROS Class") +
  theme_bw() +
  theme(legend.position = c(0.855, 0.84),
                 legend.background = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 10),
                 axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))
dev.off()

## PAPER
png("figures/ch2/paper/rosdistrcor_paper.png", width = 5, height = 5, units = "in", res = 250)
peaks %>%
  ggplot() +
  # geom_density(aes(x = mult, color = ros), linewidth = 1.3, show.legend = FALSE) +
  stat_density(aes(x = mult, color = ros),
               geom = "line", position = "identity", lwd = 1) +
  geom_density(aes(x = mult, color = "gray70"), linetype = "dashed", show.legend = FALSE,
               inherit.aes = FALSE) +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x, n = 6),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  xlab("Surge (cfs, log2-scale)") +
  ylab("Density") +
  scale_color_manual(values = c("gray70", "#bf812d", "#41ab5d"),
                     labels = c("Overall", "Non-ROS", "ROS")) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid", "solid"),
                                                  lwd = c(0.5, 1, 1)))) +
  # labs(color = "ROS Class") +
  theme_bw() +
  theme(legend.position = c(0.842, 0.88),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key=element_blank()) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
dev.off()

