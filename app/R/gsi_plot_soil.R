library(Hmisc)
library(dplyr)
library(ggplot2)

#' `data`  data.frame; data already filtered by date and site
#' `yvar`  character; the column name in data to plot on the y axis
#' NOTE data doesn't exist for all combinations of site and depth and yvar
gsi_plot_soil <- function(data, yvar) {
  plot_data <- data |> 
    mutate(month_str = lubridate::month(datetime, label = TRUE), month_num = lubridate::month(datetime)) |>
    # recode depth to make number positive and add "m", e.g. "0.5m"
    mutate(depth = paste(-depth_height_m, "m")) |> 
    filter(!is.na(.data[[yvar]])) 
  
  #separate the points a bit for better readability
  dodge <- position_dodge(width = 0.4)
  
  p <- 
    ggplot(plot_data, 
           aes(x = month_num, y = .data[[yvar]], linetype = basin, shape = basin, color = site)) +
    stat_summary(fun.data = mean_sdl, position = dodge) +
    scale_x_continuous(
      name = "month",
      breaks = \(x) round(x[1]):round(x[2]),
      labels = \(x) month(x, label = TRUE)
    ) +
    scale_color_manual(values = gsi_site_colors) + #defined in 0-theme_gsi.R
    scale_linetype_manual(values = c("y" = 1, "n" = 2)) +
    guides(color = "none") +
    theme(axis.title.x.bottom = element_blank()) +
    facet_grid(depth~site)
  
  # Add line only if more than one month.  Otherwise this errors.
  if (length(unique(plot_data$month_num)) > 1) {
    p <-
      p + 
      stat_summary(fun = mean, geom = "line", position = dodge)
  }
  
  #return:
  p
}

# gsi_plot_soil(data_filtered, yvar = "soil_temperature.value", depth = 0.15)
