library(cardidates)

# read in peaks
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")
data.table::setDT(peaks)


