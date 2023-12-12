library(dplyr)
library(ggplot2)

#' `data`: the dataset already filtered to a single site and a date range
gsi_plot_airtemp <- function(data) {
  
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM"))
  
  ggplot(data_atm, aes(x = datetime, y = air_temperature.value, color = site)) +
    geom_line() +
    #this makes the line go all the way to the edge of the plot.  I like this for timeseries
    scale_x_datetime(expand = c(0,0)) +
    labs(y = "Air Temp. (ºC)") +
    theme(axis.title.x = element_blank()) 
}


# An alternative that summarizes the hourly data to daily with daily mean, low, and high
#' `data`: the dataset already filtered to a single site and a date range
gsi_plot_airtemp_daily <- function(data) {
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM"))
  
  data_airtemp <- 
    data_atm |> 
    mutate(date = date(datetime)) |> 
    group_by(date) |> 
    summarize(
      airtemp_mean = mean(air_temperature.value, na.rm = TRUE),
      airtemp_low = min(air_temperature.value, na.rm = TRUE),
      airtemp_high = max(air_temperature.value, na.rm = TRUE)
    )
  
  ggplot(data_airtemp, aes(x = date)) +
    geom_line(aes(y = airtemp_mean), color = "darkred") +
    geom_ribbon(aes(ymin = airtemp_low, ymax = airtemp_high), fill = "darkred", alpha = 0.4) +
    scale_x_date(expand = c(0,0)) +
    labs(y = "Air Temp. (ºC)") +
    theme(axis.title.x = element_blank())
}