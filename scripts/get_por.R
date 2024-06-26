# libraries
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)

# get all periods of record for all stations
pors_all <- data.frame()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
for (x in seq_along(states)) {
  # read in streamflow data
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))

  # combine data and convert to data table
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)

  # calculate por
  pors <- rhv_all[, .(por = difftime(datetime[.N], datetime[1], units = "days") |>
                        as.numeric() / 365,
                      year_start = year(datetime[1]),
                      year_end = year(datetime[.N])),
                  by = id]

  # combine with state abb
  comb <- cbind(rep(states[x], nrow(pors)), pors)

  # add to por overall data frame
  pors_all <- rbind(pors_all, comb)
}

saveRDS(pors_all, "data-raw/por_comp.rds")
head(pors_all)
dim(pors_all)

# subset to contain pors at all gages, gages that reported peaks, and
# gages included in analysis

# all gages we have data for
pors_all <- readRDS("data-raw/por_comp.rds")
hist(pors_all$por)
hist(pors_all$year_start)
summary(pors_all$year_end)
min(pors_all$year_start)

# gages with peaks
peaks <- readRDS("data-raw/peaks_fin/peaks_base_med_ref.RDS")
pors_peaks <- pors_all[id %in% unique(peaks$id)]
png("figures/ch2/paper/por_peaks.png", width = 6, height = 3, units = "in", res = 300)
# histogram of por length
g1 <- pors_peaks |>
  ggplot(aes(x = por)) +
  geom_histogram(bins = 15, col = "gray", fill = "#023E8A", ) +
  xlab("Length of Period of Record (in years)") +
  ylab("Count") +
  scale_x_continuous(limits = c(0, 50)) +
  theme_bw()
# histogram of start year
g2 <- pors_peaks |>
  ggplot(aes(x = year_start)) +
  geom_histogram(bins = 15, col = "gray", fill = "#023E8A", ) +
  xlab("Period of Record Start Year") +
  ylab("Count") +
  scale_x_continuous(limits = c(1980, 2025), breaks = seq(1980, 2025, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.minor.x = element_blank())
g1 <- ggplotGrob(g1)
g2 <- ggplotGrob(g2)
g <- cbind(g1, g2, size = "first")
g$heights <- unit.pmax(g1$heights, g2$heights)
grid.newpage()
grid.draw(g)
dev.off()

# gages in analysis
peaks_left <- readRDS("data-raw/modeling/peak_data_sf.rds")
pors_left <- pors_all[id %in% unique(peaks_left$id)]
hist(pors_left$por)
hist(pors_left$year_start)
hist(pors_left$year_end)
min(pors_left$year_start)
png("figures/ch2/paper/por_left.png", width = 6, height = 3, units = "in", res = 300)
# histogram of por length
g1 <- pors_left |>
  ggplot(aes(x = por)) +
  geom_histogram(bins = 15, col = "gray", fill = "#023E8A", ) +
  xlab("Length of Period of Record (in years)") +
  ylab("Count") +
  scale_x_continuous(limits = c(0, 50)) +
  theme_bw()
# histogram of start year
g2 <- pors_left |>
  ggplot(aes(x = year_start)) +
  geom_histogram(bins = 15, col = "gray", fill = "#023E8A", ) +
  xlab("Period of Record Start Year") +
  ylab("Count") +
  scale_x_continuous(limits = c(1980, 2025), breaks = seq(1980, 2025, 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.minor.x = element_blank())
g1 <- ggplotGrob(g1)
g2 <- ggplotGrob(g2)
g <- cbind(g1, g2, size = "first")
g$heights <- unit.pmax(g1$heights, g2$heights)
grid.newpage()
grid.draw(g)
dev.off()





# figure out workflow
rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_UT.RDS"))
data.table::setDT(rhv_tot)
rhv_tot$datetime[136] - rhv_tot$datetime[2]
difftime(rhv_tot$datetime[4], rhv_tot$datetime[2], units = "days")

rhv_tot[, .(por = difftime(datetime[.N], datetime[1], units = "days") |>
              as.numeric() / 365,
            year_start = year(datetime[1]),
            year_end = year(datetime[.N])),
        by = id]


