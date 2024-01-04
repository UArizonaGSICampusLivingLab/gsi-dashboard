library(dplyr)
library(ggplot2)

#' `data`: the dataset already filtered to a single site and a date range
#' `daily`: plot daily summary instead of hourly data
gsi_plot_precip <- function(data, daily = FALSE) {
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM")) 
  
  if (isTRUE(daily)) {
    data_atm <-
      data_atm |> 
      mutate(datetime = floor_date(datetime, "day")) |> 
      dplyr::summarise(
        precipitation.value = sum(precipitation.value, na.rm = TRUE),
        .by = c(site, datetime)
      ) |> 
      #becaue sum(c(NA, NA, NA), na.rm = TRUE) results in Inf, we need to replace these with regular NAs for better plotting
      mutate(precipitation.value = if_else(!is.finite(precipitation.value), NA, precipitation.value))
  }
  
  ggplot(data_atm, aes(x = datetime, y = precipitation.value, fill = site, color = site)) +
    geom_col(position = position_dodge2(padding = 0.2, preserve = "total")) +
    scale_x_datetime(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") + #turn off legend
    labs(y = "Precipitation (mm)") +
    theme(axis.title.x = element_blank()) 
}

