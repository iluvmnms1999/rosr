# libraries

# get all periods of record for all stations
pors_all <- data.frame()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ")
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

# subset to just contain pors at gages that reported peaks

# make viz of distribution


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


