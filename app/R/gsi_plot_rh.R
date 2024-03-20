library(dplyr)
library(ggplot2)

#' `data`: the dataset already filtered to a single site and a date range
#' `daily`: display a daily summary instead of hourly data
gsi_plot_rh <- function(data, daily = FALSE) {
  #use just atmospheric sensor data for this plot
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM")) 
  
  if (isTRUE(daily)) {
    data_atm <-
      data_atm |> 
      mutate(datetime = floor_date(datetime, "day")) |> 
      dplyr::summarize(
        rh_mean = mean(relative_humidity.value, na.rm = TRUE),
        rh_min = min(relative_humidity.value, na.rm = TRUE),
        rh_max = max(relative_humidity.value, na.rm = TRUE),
        .by = c(site, datetime)
      ) |> 
      #because mean(c(NA, NA, NA), na.rm = TRUE) results in Inf, we need to replace these with regular NAs for better plotting
      mutate(across(starts_with("rh_"), \(x) if_else(!is.finite(x), NA, x)))
    
    p <- 
      ggplot(data_atm, aes(x = datetime)) +
      geom_line(aes(y = rh_mean, color = site), linewidth = 1) +
      geom_ribbon(aes(ymin = rh_min, ymax = rh_max, fill = site), alpha = 0.2)
  } else {
    p <- 
      ggplot(data_atm, aes(x = datetime, color = site)) +
      geom_line(aes(y = relative_humidity.value), linewidth = 1.5, alpha = 0.75, na.rm = TRUE)
    
  }
  
  p +
    #this makes the line go all the way to the edge of the plot.  I like this for timeseries
    scale_x_datetime(expand = c(0,0)) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") + #turn off legend
    theme(axis.title.x = element_blank(), legend.position = "top") +
    labs(y = "Relative Humidity")
}


# #example:
# site_info <- read_csv("metadata.csv")
# data_full <-
#   read_csv("data/gsi_living_lab_data.csv") |>
#   right_join(site_info, by = join_by(device_sn, sensor, port))
# data_filtered <-
#   data_full |>
#   filter(site == "Old Main", datetime > "2023-11-01", datetime < "2023-11-15")
# gsi_plot_rh(data_filtered)
