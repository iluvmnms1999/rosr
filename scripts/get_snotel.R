library(data.table)
library(anytime)
library(httr)
library(dplyr)
library(tidyr)
source("R/download_snotel_ind.R")

snotel_id <- read.csv("data-raw/snotel_id.csv")
data.table::setDT(snotel_id)
snotel_id[, .N, by = state]

# get number id's for stations
num_id <- stringr::str_extract(snotel_id$site_name, "(\\d+)")

# get vector of all id's to input to download_snotel
site_vec <- c()
for (i in seq_along(snotel_id$site_name)) {
  site_vec[i] <- paste0(num_id[i], ":", snotel_id$state[i], ":SNTL")
}
site_vec

snotel_id <- cbind(site_vec, snotel_id)

# download for one station
states <- c("CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
for (i in seq_along(states)) {
  stations <- snotel_id[snotel_id$state == states[i]]$site_vec
  snotel <- download_snotel(freq = "daily",
                            destpath = paste0("data-raw/snotel/", states[i],
                                              "_snotel"),
                            sites = stations,
                            begin_date = as.Date("1980-01-02"),
                            end_date = as.Date("2023-09-07")
  )
  saveRDS(snotel, paste0("data-raw/snotel/snotel_fin_", states[i], ".RDS"))
}



## check things out and figure out if there's a better way to download
az <- readRDS("data-raw/snotel/snotel_fin_AZ2.RDS")
ca <- readRDS("data-raw/snotel/snotel_fin_CA2.RDS")
co <- readRDS("data-raw/snotel/snotel_fin_CO2.RDS")
id <- readRDS("data-raw/snotel/snotel_fin_ID2.RDS")
mt <- readRDS("data-raw/snotel/snotel_fin_MT2.RDS")
nm <- readRDS("data-raw/snotel/snotel_fin_NM2.RDS")
nv <- readRDS("data-raw/snotel/snotel_fin_NV2.RDS")
or <- readRDS("data-raw/snotel/snotel_fin_OR2.RDS")
ut <- readRDS("data-raw/snotel/snotel_fin_UT2.RDS")
wa <- readRDS("data-raw/snotel/snotel_fin_WA2.RDS")
wy <- readRDS("data-raw/snotel/snotel_fin_WY2.RDS")

cols <- c("id", "date", "air_temperature_observed_degc_start_of_day_values",
          "precipitation_increment_mm", "snow_depth_cm_start_of_day_values",
          "snow_water_equivalent_mm_start_of_day_values",
          "soil_moisture_percent_2in_pct_start_of_day_values",
          "soil_moisture_percent_8in_pct_start_of_day_values",
          "soil_moisture_percent_20in_pct_start_of_day_values"
)

az2 <- az %>%
  select(all_of(cols))
ca2 <- ca %>%
  select(all_of(cols))
co2 <- co %>%
  select(all_of(cols))
id2 <- id %>%
  select(all_of(cols))
mt2 <- mt %>%
  select(all_of(cols))
nm2 <- nm %>%
  select(all_of(cols))
nv2 <- nv %>%
  select(all_of(cols))
or2 <- or %>%
  select(all_of(cols))
ut2 <- ut %>%
  select(all_of(cols))
wa2 <- wa %>%
  select(all_of(cols))
wy2 <- wy %>%
  select(all_of(cols))

readRDS(az2)

# make sure snotel hucs line up
snotel <- readRDS("data-raw/snotel/snotel_huc.RDS")
head(snotel)

vec <- c()
for (i in seq_along(snotel$huc8)) {
  vec[i] <- grepl(snotel$huc8[i], snotel$huc[i])
}

nrow(snotel[which(vec == FALSE),]) # 48 hucs don't agree



## just keep important variables for snotel -- snotel_clean
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa", "wy"))

for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/snotel_og_", states[i], ".RDS"))
  temp <- snotel[, c("id", "date", "air_temperature_observed_degc_start_of_day_values",
                 "precipitation_increment_mm", "snow_depth_cm_start_of_day_values",
                 "snow_water_equivalent_mm_start_of_day_values")]
  names(temp) <- c("id", "date", "temp_degc", "prec", "snow_dep", "swe")
  saveRDS(names, paste0("data-raw/snotel/snotel_clean_", states[i], ".RDS"))
}
