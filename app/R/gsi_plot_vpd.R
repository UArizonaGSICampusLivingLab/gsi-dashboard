library(dplyr)
library(ggplot2)

#' `data`: the dataset already filtered to a single site and a date range
#' `daily`: display a daily summary instead of hourly data
gsi_plot_vpd <- function(data, daily = FALSE) {
  #use just atmospheric sensor data for this plot
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM")) 
  
  if (isTRUE(daily)) {
    data_atm <-
      data_atm |> 
      mutate(datetime = floor_date(datetime, "day")) |> 
      dplyr::summarize(
        across(c(vapor_pressure.value, vpd.value), \(x) mean(x, na.rm = TRUE)),
        .by = c(site, datetime)
      ) |> 
      #because mean(c(NA, NA, NA), na.rm = TRUE) results in Inf, we need to replace these with regular NAs for better plotting
      mutate(across(c(vapor_pressure.value, vpd.value), \(x) if_else(!is.finite(x), NA, x)))
  }
  
  data_atm
  
  gg <- ggplot(data_atm, aes(x = datetime, color = site)) +
    #rather than map color to a column in the data or set it manually, we do a
    #third thing—set it to a character string *inside* of aes().  This is a
    #"trick" for creating a color legend
    geom_line_interactive(aes(y = vapor_pressure.value, linetype = "VP"), linewidth = 0.65, alpha = 0.5, na.rm = TRUE) +
    geom_line_interactive(aes(y = vpd.value, linetype = "VPD"), linewidth = 0.65, alpha = 0.5, na.rm = TRUE) +
    #this makes the line go all the way to the edge of the plot.  I like this for timeseries
    scale_x_datetime(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors) + #defined in 0-theme_gsi.R
    guides(color = "none") + #turn off legend
    theme(axis.title.x = element_blank(), legend.position = "top") +
    labs(y = "Vapor Pressure / VPD (kPa)")
girafe(ggobj = gg)
  }


# #example:
# site_info <- read_csv("metadata.csv")
# data_full <-
#   read_csv("data/gsi_living_lab_data.csv") |>
#   right_join(site_info, by = join_by(device_sn, sensor, port))
# data_filtered <-
#   data_full |>
#   filter(site == "Old Main", datetime > "2023-11-01", datetime < "2023-11-15")
# gsi_plot_vpd(data_filtered)
