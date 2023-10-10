peak_plot <- function(station, peaks, beg_date, end_date) {
  # filter data frame for selected station
  sub <- dplyr::filter(rhv_tot, id == as.character(station) &
                         dplyr::between(datetime,
                                        lubridate::ymd_h(beg_date,
                                                         tz = "US/Pacific"),
                                        lubridate::ymd_h(end_date,
                                                         tz = "US/Pacific")
                         )
  )

  # add points for defined peaks
  peaks_df <- df_peaks_filt(station, peaks)

  # set maximum of y axis
  y_max <- max(dplyr::filter(peaks_df,
                             dplyr::between(dt,
                                            lubridate::ymd_h(beg_date,
                                                             tz = "US/Pacific"),
                                            lubridate::ymd_h(end_date,
                                                             tz = "US/Pacific")
                             )
  )$y
  )

  # create plot of data and peak classification
  ggplot2::ggplot(sub, ggplot2::aes(x = as.POSIXct(datetime),
                                    y = max_flow)) +
    geom_point(size = .2, col = "grey50") +
    ggtitle(paste0("Streamflow Peaks at Station ",
                   as.character(station))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("DateTime") +
    scale_x_datetime(
      limits = c(lubridate::ymd_h(beg_date, tz = "US/Pacific"),
                 lubridate::ymd_h(end_date, tz = "US/Pacific")),
      date_breaks = "1 week",
      date_labels = "%m-%d-%y"
    ) +
    ylab("Discharge amount (ft^3 per sec)") +
    scale_y_continuous(
      limits = c(0, ifelse(y_max + 0.2 * y_max > 20000,
                           plyr::round_any(y_max + 0.2 * y_max, 10000),
                           ifelse(y_max + 0.2 * y_max > 2000,
                                  plyr::round_any(y_max + 0.2 * y_max, 1000),
                                  ifelse(y_max + 0.2 * y_max > 200,
                                         plyr::round_any(y_max + 0.2 * y_max,
                                                         100),
                                         plyr::round_any(y_max + 0.2 * y_max,
                                                         10))))),
      n.breaks = 5, breaks = waiver()) +
    geom_point(data = peaks_df, aes(as.POSIXct(dt), y),
               color = ifelse(peaks_df$type %in% c("major", "flood"),
                              "red3",
                              ifelse(peaks_df$type == "minor",
                                     "gold", "green3")
               )
    )
}
