library(Hmisc)
library(dplyr)
library(ggplot2)

#' `data`  data.frame; data already filtered by date and site
#' `yvar`  character; the column name in data to plot on the y axis
#' NOTE data doesn't exist for all combinations of site and depth and yvar
gsi_plot_soil <- function(data, yvar) {
  data |> 
    mutate(month = lubridate::month(datetime, label = TRUE)) |> 
    # recode depth
    mutate(depth = paste(-depth_height_m, "m")) |> 
    
    filter(!is.na(.data[[yvar]])) |> 
    ggplot(aes(x = month, y = .data[[yvar]], linetype = basin, shape = basin, color = site)) +
    stat_summary(fun.data = mean_sdl, position = position_dodge(width = 0.7)) +
    
    # This adds a line, which I quite like, but causes issues with only one month selected AND requires the months to be numeric, which requires scale_x_continuous to fix
    # stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.7)) +
    # scale_x_continuous(labels = \(x) month(x, label = TRUE)) +
    
    #TODO: this is in the PR for the atmospheric tab
    # scale_color_manual() + 
    scale_linetype_manual(values = c("y" = 1, "n" = 2)) +
    guides(color = "none") +
    theme(axis.title.x.bottom = element_blank()) +
    facet_wrap(~depth, labeller = label_both)
}

## to plug into shiny app, !!as.name("character_string")
# gsi_plot_soil(data_filtered, yvar = "soil_temperature.value", depth = 0.15)
