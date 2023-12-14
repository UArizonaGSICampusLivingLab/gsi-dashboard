library(Hmisc)
library(dplyr)
library(ggplot2)

#' `data`  data.frame; data already filtered by date and site
#' `yvar`  character; the column name in data to plot on the y axis
#' NOTE data doesn't exist for all combinations of site and depth and yvar
gsi_plot_soil <- function(data, yvar) {
  plot_data <- data |> 
    mutate(month_str = lubridate::month(datetime, label = TRUE), month_num = lubridate::month(datetime)) |>
    # recode depth
    mutate(depth = paste(-depth_height_m, "m")) |> 
    filter(!is.na(.data[[yvar]])) 
  
  p <- 
    ggplot(plot_data, 
           aes(x = month_num, y = .data[[yvar]], linetype = basin, shape = basin, color = site)) +
    stat_summary(fun.data = mean_sdl, position = position_dodge(width = 0.7)) +
    scale_x_continuous(
      name = "month",
      breaks = \(x) round(x[1]):round(x[2]),
      labels = \(x) month(x, label = TRUE)
    ) +
  
  #TODO: this is in the PR for the atmospheric tab
  # scale_color_manual() + 
  scale_linetype_manual(values = c("y" = 1, "n" = 2)) +
    guides(color = "none") +
    theme(axis.title.x.bottom = element_blank()) +
    facet_wrap(~depth, labeller = label_both, ncol = 1, strip.position = "right")
  
  # Add line only if more than one month.  Otherwise this errors.
  if (length(unique(plot_data$month_num)) > 1) {
    p <-
      p + 
      stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.7))
  }
  
  #return:
  p
}

## to plug into shiny app, !!as.name("character_string")
# gsi_plot_soil(data_filtered, yvar = "soil_temperature.value", depth = 0.15)
