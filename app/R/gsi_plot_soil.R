library(Hmisc)
library(dplyr)
library(ggplot2)

#' `data`  data.frame; data already filtered by date and site
#' `yvar`  character; the column name in data to plot on the y axis
#' `depth` numeric; either `0.5` or `0.15`
#' NOTE data doesn't exist for all combinations of site and depth and yvar
gsi_plot_soil <- function(data, yvar, depth) {
  data |> 
    filter(depth_height_m == -depth) |> 
    mutate(month = month(datetime)) |> 
    filter(!is.na(.data[[yvar]])) |> 
    ggplot(aes(x = month, y = .data[[yvar]], linetype = basin, shape = basin, color = site)) +
    stat_summary(fun.data = mean_sdl, position = position_dodge(width = 0.7)) +
    stat_summary(fun.y = mean, geom = "line", position = position_dodge(width = 0.7)) +
    scale_x_continuous(labels = \(x) month(x, label = TRUE)) +
    # scale_color_manual() + #TODO: this is in the PR for the atmospheric tab
    scale_linetype_manual(values = c("y" = 1, "n" = 2)) +
    guides(color = FALSE, linetype = FALSE) +
    theme(axis.title.x.bottom = element_blank()) +
    theme(legend.position = "top")
}

## to plug into shiny app, !!as.name("character_string")
gsi_plot_soil(data_full, yvar = "soil_temperature.value", depth = 0.15)

