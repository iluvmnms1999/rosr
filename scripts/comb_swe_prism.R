# libraries
# library(utils)
# library(R.utils)
# library(lubridate)
# library(stars)
# source("R/download_prism_ind.R")
library(dplyr)
library(prism)

# organize prism data into one data frame
prism_all <- list.files(path = paste0(getwd(), "/data-raw/prism/exprismcsv"),  # Identify all CSV files
                        pattern = "*.csv", full.names = TRUE) %>%
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows
data.table::setDT(prism_all)
num_id <- stringr::str_extract(prism_all$snotel...3., "(\\d+)")
prism_all <- cbind(num_id, prism_all)
prism_all$date <- as.Date(prism_all$date)
prism_all <- prism_all[, c(1, 4, 5)]
names(prism_all) <- c("num_id", "date", "prec_prism")
prism_all$num_id <- as.numeric(prism_all$num_id)
saveRDS(prism_all, "data-raw/prism/prism_all.RDS")

# add prism precip to snotel data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "WY") #AZ
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_",
                           states[i], ".RDS"))
  snotel$date <- as.Date(snotel$date)
  num_id <- stringr::str_extract(snotel$id, "(\\d+)")
  snotel2 <- cbind(num_id, snotel)
  snotel2$num_id <- as.numeric(snotel2$num_id)
  add_prism <- left_join(snotel2, prism_all, by = c("num_id", "date"))
  add_prism <- add_prism[, c(2:5, 14, 12, 6:11, 13)]
  names(add_prism)[4] <- "prec_snotel"
  # sort data frame by station
  # shift prism values up one to match with snotel
  add_prism_ord <- add_prism[order(add_prism$id),]
  add_prism_ord2 <- add_prism_ord %>%
    group_by(id) %>%
    mutate(prec_prism = lead(prec_prism))

  add_prism_diff2 <- add_prism_ord2 %>%
    group_by(id) %>%
    mutate(melt_prism = swe + prec_prism - lead(swe))

  # make negative melt values 0
  add_prism_diff2$melt[which(add_prism_diff2$melt < 0)] <- 0
  add_prism_diff2 <- ungroup(add_prism_diff2)
  prism_fin <- add_prism_diff2[, c(1:6, 14, 7:13)]
  names(prism_fin)[6] <- "melt_snotel"
  data.table::setDT(add_prism_diff2)
  saveRDS(add_prism_diff2, paste0("data-raw/prism/snotel_prism_comb/comb_w_prism_",
                                  states[i], ".RDS"))
}














# clean up snotel
snotel_ddf <- download_snotel(freq = "daily",
                              destpath = "data/snotel/snotel_ddf",
                              sites = c("1051:CA:SNTL", "1049:CA:SNTL",
                                        "1050:CA:SNTL", "633:CA:SNTL",
                                        "778:CA:SNTL", "697:CA:SNTL",
                                        "462:CA:SNTL"),
                              begin_date = as.Date("1991-01-01"),
                              end_date = Sys.Date()
)

## bind prism prec measurements to snotel swe to find change variable
# get swe measurements
swe_snotel_df <- snotel_ddf[, c(1:2, 6)]
names(swe_snotel_df) <- c("id", "date", "swe")

# clean prec_snotel_df to only have digit id's
swe_snotel_df$id <- as.numeric(substr(formatC(swe_snotel_df$id,
                                              width = 4,
                                              format = "d",
                                              flag = "0"
), 1, 4
)
)

# put both prism and snotel prec amounts in same data frame
head(data_all)
class(data_all$date)
class(x$date)

swe_snotel_df$date <- as.Date(swe_snotel_df$date)
swe_prec <- left_join(data_all, swe_snotel_df, by = c("id", "date"))

names(swe_prec) <- c("id", "date", "prec_prism", "swe_snotel")
