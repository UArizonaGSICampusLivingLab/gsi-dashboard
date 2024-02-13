library(dplyr)
library(ggplot2)

#' `data`: the dataset already filtered to a single site and a date range
#' `daily`: plot a daily summary instead of hourly data
gsi_plot_airtemp <- function(data, daily = FALSE) {
  
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM"))
  
  if (isTRUE(daily)) {
    data_atm <- 
      data_atm |> 
      mutate(date = floor_date(datetime, "day")) |>
      dplyr::summarize(
        airtemp_mean = mean(air_temperature.value, na.rm = TRUE),
        airtemp_low = min(air_temperature.value, na.rm = TRUE),
        airtemp_high = max(air_temperature.value, na.rm = TRUE),
        .by = c(site, date)
      ) |> 
      #becaue min(c(NA, NA, NA), na.rm = TRUE) results in Inf, we need to replace these with regular NAs for better plotting
      mutate(across(starts_with("airtemp_"), \(x) if_else(!is.finite(x), NA, x)))
  }
  
  
  if (isTRUE(daily)) {
    p <- 
      ggplot(data_atm, aes(x = date, y = airtemp_mean, ymin = airtemp_low, ymax = airtemp_high)) +
      geom_line(aes(color = site), linewidth = 1) + #line width from .65 to 1
      geom_ribbon(aes(fill = site), alpha = 0.2) # alpha from .4 to .2
    
  } else {
    p <-
      ggplot(data_atm, aes(x = datetime, y = air_temperature.value, color = site)) +
      geom_line(alpha = 0.75, linewidth = 1.5) #line width from .65 to 1  #transparency to .25 from .5
  }

  p +
    #this makes the line go all the way to the edge of the plot.  I like this for timeseries
    scale_x_datetime(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") + #turn off legend
    labs(y = "Air Temp. (ºC)") +
    theme(axis.title.x = element_blank())   
}

