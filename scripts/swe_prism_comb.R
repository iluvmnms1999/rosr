# libraries
# library(utils)
# library(R.utils)
# library(lubridate)
# library(stars)
# source("R/download_prism_ind.R")
library(prism)

# organize prism data into friendlier format
data_all <- list.files(path = paste0(getwd(), "/data/exprism"),  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>%
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows

###### download prism using prism package
prism::prism_set_dl_dir("data-raw/prism")

get_prism_dailys(
  type = "ppt",
  minDate = "1981-10-11",
  maxDate = "2023-08-30",
  keepZip = FALSE
)

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
swe_snotel_df$date <- as.Date(swe_snotel_df$date)
swe_prec <- left_join(data_all, swe_snotel_df, by = c("id", "date"))

names(swe_prec) <- c("id", "date", "prec_prism", "swe_snotel")
