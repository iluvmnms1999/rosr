# read in max hourly measurements
rhv_tot <- data.table::fread("data-raw/rhv_tot.csv")
usgs_fs_cl <- data.table::fread("data-raw/usgs_fs_cl.csv")

undef <- usgs_fs_cl[is.na(discharge)]

for (i in seq_along(undef$site_no)) {
  # subset on station id
  sub <- rhv_tot[id == undef$site_no[i]]
  # add year to data table
  sub[, year := data.table::year(datetime)]
  # get ann_max
  sub[, .(ann_max = max(max_flow)), by = year]
}
sub <- filter(shv_tot, id == "10308200")
sub$year <- year(sub$datetime)
tib <- sub %>% group_by(year) %>% summarize(ann_max = max(max_flow))
prop <- quantile(tib$ann_max, .75) / max(tib$ann_max)
prop
max(tib$ann_max) * prop
