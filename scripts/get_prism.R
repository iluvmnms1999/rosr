library(stars)
library(terra)
library(exactextractr)
library(lubridate)
library(utils)
library(R.utils)
source("R/download_prism_ind.R")

years <- seq(1987, 2022, 1)

# save prism data as rasters in specified folders
for (i in seq_along(years)) {
  download_prism(
    sp_res = "4km",
    data = "ppt",
    start_date = as.Date(paste0(as.character(years[i]), "-10-01")),
    end_date = as.Date(paste0(as.character(years[i + 1]), "-09-30")),
    t_res = "daily",
    out_dir = paste0(getwd(), "/data-raw/prism/", as.character(years[i])
    )
  )
}

# read prism data as raster
prism_ppt_19810102 <- terra::rast("data-raw/prism/2014/PRISM_ppt_stable_4kmD2_20150201_bil.bil")
terra::plot(prism_ppt_19810102)

# read in SNOTEL data frame with station coordinates
snotel <- readRDS("data-raw/snotel/snotel_huc.RDS")

prism_snotel_crds <- terra::extract(prism_ppt_19810102, as.data.frame(snotel[, c(7, 6)]))
date <- rep(19841001, nrow(snotel))
x <- cbind(snotel[,3], date, prism_snotel_crds[, 2])



# start prism data extraction here
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
years <- seq(1984, 2021, 1)
snotel <- readRDS("data-raw/snotel/snotel_huc.RDS")
# write extraction function
prism_extract <- function(years, snotel) {
  lst1 <- vector("list", length = length(years))
  for (i in seq_along(years)) {
    # create date vector
    dates <- seq(as.Date(paste0(as.character(years[i]), "-10-01"), tz = "US/Pacific"),
                 as.Date(paste0(as.character(years[i] + 1), "-09-30"), tz = "US/Pacific"),
                 by = "day")
    lst2 <- vector("list", length = length(dates))
    for (j in seq_along(dates)){
      # create raster from .bil file for each day
      temp <- terra::rast(paste0(getwd(), "/data-raw/prism/", as.character(years[i]),
                                 "/PRISM_ppt_stable_4kmD2_",
                                 as.character(year(dates[j])),
                                 formatC(month(dates[j]),
                                         width = 2,
                                         format = "d",
                                         flag = "0"),
                                 formatC(day(dates[j]),
                                         width = 2,
                                         format = "d",
                                         flag = "0"),
                                 "_bil.bil")
      )

      prism_snotel_crds <- terra::extract(temp, snotel[, c(7, 6)])
      date <- rep(dates[j], nrow(snotel))
      lst2[[j]] <- data.frame(snotel[,3], date, prism_snotel_crds[, 2])
    }
    lst1[[i]] <- lst2
  }
  lst1
}

prism_precip <- prism_extract(years, as.data.frame(snotel))

lst3 <- vector("list", length = length(prism_precip))
for (i in seq_len(length(prism_precip))) {
  lst3[[i]] <- do.call(rbind, prism_precip[[i]])
}

for (i in seq_len(length(lst3))) {
  write.csv(lst3[[i]], paste0("data-raw/prism/exprismcsv/", as.character(years[i]), ".csv"))
}


y <- readRDS("data-raw/prism/exprism/1987.RDS")


## cropping to nv
# get nevada shape file for extraction
nv <- sf::st_read(
  dsn = "data/nv_shape",
  layer = "nv_county"
)
upper_carson <- nv[nv$NAME %in% c("Douglas", "Carson City", "Lyon", "Storey"),]

prism_ppt_19931001 <- terra::rast("data/prism/1993/PRISM_ppt_stable_4kmD2_19931001_bil.bil")

# extract data from raster
precip_19931001 <- exactextractr::exact_extract(prism_ppt_19931001,
                                                upper_carson,
                                                include_cols = c("NAME",
                                                                 "FIPS")
)

names(snotel_ddf) <- c("id", "date", "temp", "precip", "sn_depth", "swe", "smp2", "smp8", "smp20")

