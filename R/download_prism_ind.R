download_prism <- function(sp_res = "4km", # or 800m
                           data = "ppt", # c("ppt", c("tmin", "tmax", "tmean")
                           start_date = as.Date("2017-04-01"),
                           end_date = as.Date("2017-08-03"),
                           t_res = "monthly", # monthly, yearly
                           out_dir = paste0(getwd())) {

  # Testing to ensure variables are in the correct format
  if ((sp_res != "4km") & (sp_res != "800m")) {
    stop("spatial resolution argument must be 4km or 800m.")
  }

  # Double-checking that every element in data is one of the products
  data <- tolower(data)
  for (i in seq_len(length(data))) {
    if (!(data[i] %in% c(
      "ppt", "tmin", "tmax", "tmean",
      "tdmean", "vpdmin", "vpdmax"
    ))) {
      stop("all data argument(s) must be a valid data option")
    }
  }

  # If no end_date, then the start date is the end date as well.
  if (missing(end_date)) {
    end_date <- start_date
  }

  time_var <- c("daily", "monthly", "yearly")
  time_va <- c("day", "month", "year")
  time_arg <- match(t_res, time_var)

  # get the first and last year, this will be used in the "for" loop
  t <- seq(start_date, end_date, by = time_va[time_arg])

  # For loops depend on time resolution
  if (t_res == "daily") {
    time_resolution <- "day"
    tdate <- seq(start_date, end_date, by = "day")
    years <- gsub("-", "", substring(tdate, 1, 4))
    tdate <- gsub("-", "", tdate)
  } else if (t_res == "monthly") {
    time_resolution <- "month"
    # } else if (t_res == "monthly") {
    tdate <- seq(start_date, end_date, by = "month")
    years <- gsub("-", "", substring(tdate, 1, 4))

    # remove the day argument and get rid of the "-" and return a 6 character
    tdate <- gsub("-", "", substring(tdate, 1, 7))
  } else if (t_res == "yearly") {
    time_resolution <- "year"
    # } else if (t_res == "yearly") {
    tdate <- seq(start_date, end_date, by = "year")
    years <- gsub("-", "", substring(tdate, 1, 4))
    tdate <- unique(gsub("-", "", substring(tdate, 1, 4)))
  }

  tsource <- c()
  destination <- c()
  tagname <- c()
  final_location <- c()

  l <- 1
  for (var in data) {
    for (j in seq_len(length(years))) {
      # Creates the vector tdate with all the dates of interest
      tsource[l] <- paste("http://services.nacse.org/prism/data/public",
                          sp_res, var, tdate[j],
                          sep = "/"
      )
      destination[l] <- out_dir
      tagname[l] <- paste("PRISM", var, sp_res, tdate[j], sep = "_")
      final_location[l] <- paste0(destination[l], "/", tagname[l], ".zip")
      l <- l + 1
    }
  }


  # Create directory of each file if it doesn't exist.
  # - https://stackoverflow.com/questions/4216753/
  # - check-existence-of-directory-and-create-if-doesnt-exist
  for (i in seq_len(length(destination))) {
    if (!dir.exists(destination[i])) {
      dir.create(destination[i], recursive = TRUE)
    }
  }

  # go through and download, unzip and remove the zipped file
  for (i in seq_len(length(tsource))) {
    print(paste("Downloading", time_resolution, i, "of", length(tsource)))
    try(utils::download.file(tsource[i],
                             final_location[i],
                             mode = "wb"
    ))
    try(utils::unzip(final_location[i],
                     exdir = destination[i]
    ))
    try(file.remove(final_location[i]))
    Sys.sleep(1)
  }
}

