# read in max hourly measurements
rhv_tot_WY <- readRDS("data-raw/rhv_tot_WY.RData")
data.table::setDT(rhv_tot_WY)
usgs_fs_cl <- data.table::fread("data-raw/usgs_fs_fin.csv")
usgs_fs_WY <- usgs_fs_cl[state == "WY"]

def <- usgs_fs_WY[!is.na(discharge)]
undef <- usgs_fs_WY[is.na(discharge)]

# to figure out general trend in proportion for defined thresholds
prop_vec1 <- c()
for (i in seq_along(def$site_no)) {
  # subset on station id
  sub <- rhv_tot_WY[id == formatC(def$site_no[i],
                                  width = 8,
                                  format = "d",
                                  flag = "0")]
  # add year to data table
  sub[, year := data.table::year(datetime)]
  # get ann_max
  tib <- sub[, .(ann_max = max(max_flow)), by = year]
  prop <- def$discharge[i] / max(tib$ann_max)
  prop_vec1[i] <- prop
}
median(prop_vec1, na.rm = TRUE)

# to get thresholds for undefined
prop_vec2 <- c()
for (i in seq_along(undef$site_no)) {
  # subset on station id
  sub <- rhv_tot_WY[id == formatC(undef$site_no[i], width = 8, format = "d", flag = "0")]
  # add year to data table
  sub[, year := data.table::year(datetime)]
  # get ann_max
  tib <- sub[, .(ann_max = max(max_flow)), by = year]
  prop <- quantile(tib$ann_max, 0.95) / max(tib$ann_max)
  prop_vec2[i] <- prop
}
median(prop_vec2, na.rm = TRUE)

# actual cutoff value
max(tib$ann_max) * prop

# maybe look into pracma::findpeaks for peak detection because you can specify
# threshold instead of relying on relationship to maximum



sub <- filter(shv_tot, id == "10308200") shv_tot[id == site_no[i]]
sub$year <- year(sub$datetime)
tib <- sub %>% group_by(year) %>% summarize(ann_max = max(max_flow))
prop <- quantile(tib$ann_max, .75) / max(tib$ann_max)
prop
max(tib$ann_max) * prop
