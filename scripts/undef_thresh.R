# read in max hourly measurements
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY")
states <- c("AZ", "WA")
usgs_fs_cl <- readRDS("data-raw/usgs_fs_comp3.RDS")
data.table::setDT(usgs_fs_cl)
usgs_fs_cl <- usgs_fs_cl[, est := is.na(discharge)]
usgs_fs_cl <- usgs_fs_cl[, minpeak := rep(0, length = nrow(usgs_fs_cl))]

for (i in seq_along(states)) {
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[i], ".RDS"))
  data.table::setDT(rhv_tot)
  usgs_fs <- usgs_fs_cl[state == states[i]]

  def <- usgs_fs[!is.na(discharge)]

  # to figure out general trend in proportion for defined thresholds
  prop_est <- c()
  for (j in seq_along(def$site_no)) {
    # subset on station id
    temp <- rhv_tot[id == formatC(def$site_no[j],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    # add year to data table
    temp[, year := data.table::year(datetime)]
    # get ann_max
    tib <- temp[, .(ann_max = max(max_flow)), by = year]
    prop <- def$discharge[j] / max(tib$ann_max)
    prop_est[j] <- prop
  }
  med <- median(prop_est, na.rm = TRUE)

  # use median proportion to estimate flood stages for other stations and add
  # minpeaks props
  vec <- c()
  props <- c()
  for (k in seq_along(usgs_fs$discharge)) {
    sub <- rhv_tot[id == formatC(usgs_fs$site_no[k],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    if (!is.na(usgs_fs$discharge[k])) {
      vec[k] <- usgs_fs$discharge[k]
    } else {
      # vec[k] <- round(med * max(sub$max_flow[sub$max_flow != max(sub$max_flow)]),
      # digits = 2) # code for finding proportion compared to second highest max
      vec[k] <- round(med * max(sub$max_flow), digits = 2) # using max
    }
  }
  vec[vec < 0] <- NA
  usgs_fs_cl[state == states[i]]$discharge <- vec

  usgs_fs <- usgs_fs_cl[state == states[i]]
  props <- c()
  for (x in seq_along(usgs_fs$site_no)) {
    sub <- rhv_tot[id == formatC(usgs_fs$site_no[x],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    props[x] <- usgs_fs$discharge[x] / max(sub$max_flow)
  }
  usgs_fs_cl[state == states[i]]$minpeak <- props
}

# just need to do it for WA and AZ and then we'll have everything and know which
# ones need to be redone
saveRDS(usgs_fs_cl, "data-raw/usgs_fs_comp4.RDS")


# maybe look into pracma::findpeaks for peak detection because you can specify
# threshold instead of relying on relationship to maximum



sub <- filter(shv_tot, id == "10308200") shv_tot[id == site_no[i]]
sub$year <- year(sub$datetime)
tib <- sub %>% group_by(year) %>% summarize(ann_max = max(max_flow))
prop <- quantile(tib$ann_max, .75) / max(tib$ann_max)
prop
max(tib$ann_max) * prop

# test peakwindow and findpeaks
date <- seq(as.Date("2014-01-01"), as.Date("2014-12-31"), by = "day")
val <- sample(1:1000, 365)
df <- data.frame(date, val)
cardidates::peakwindow(df, minpeak = 1.24) # does work with proportions higher than 1

peaks <- pracma::findpeaks(df$val, minpeakheight = 750, nups = 2)
peaks <- as.data.frame(peaks)
peaks$V5 <- df$date[peaks$V2]

peak_plot <- function(x, beg_date, end_date) {
  # create plot of data and peak classification
  ggplot2::ggplot(x, ggplot2::aes(x = date,
                                  y = val)) +
    geom_point(size = .2, col = "grey50") +
    xlab("Date") +
    ylab("Peak") +
    geom_point(data = peaks, aes(V5, V1)
    )
}

peak_plot(df, "2014-01-01", "2014-03-01")
