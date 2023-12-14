library(dplyr)
library(ggplot2)

#' `data`: the dataset already filtered to a single site and a date range
gsi_plot_precip <- function(data) {
  data_atm <- 
    data |> 
    filter(str_starts(sensor, "ATM")) 
  
  ggplot(data_atm, aes(x = datetime, y = precipitation.value, fill = site, color = site)) +
    geom_col(position = position_dodge2(padding = 0.2, preserve = "total")) +
    scale_x_datetime(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") + #turn off legend
    labs(y = "Precipitation (mm)") +
    theme(axis.title.x = element_blank()) 
}

