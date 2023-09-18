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

