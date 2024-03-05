gsi_plot_temp_adj <- function(data) {
  
  plot_df <-
    data |> 
    filter(sensor_model == "ATMOS41") |> 
    mutate(adjustment = case_when(
      air_temperature_adj.value > air_temperature.value ~ "hotter",
      air_temperature_adj.value < air_temperature.value ~ "colder",
      .default = NA
    )) |> 
    mutate(
      temp_adj = ifelse(air_temperature_adj.value == air_temperature.value, NA, air_temperature_adj.value)
    )
  
  #Plot up or down triangle if adjusted temp is hotter or colder than real temp
  p_base <- 
    ggplot(plot_df, aes(x = datetime, color = site)) +
    geom_line(aes(y = air_temperature.value), alpha = 0.5, linewidth = 1) + 
    
    ## another option is to just plot shapes instead of arrows
    # geom_point(aes(shape = adjustment, y = air_temperature_adj.value, fill = site),
    #            alpha = 0.7) +
    # scale_shape_manual(values = c("hotter" = 24, "colder" = 25)) +
    
    scale_x_datetime(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    labs(y = "Temperature (ºC)") +
    theme(axis.title.x = element_blank(), legend.position = "none")
  
  if (any(!is.na(plot_df$temp_adj))) {
    p_base +
      geom_segment(aes(y = air_temperature.value, yend = temp_adj, xend = datetime),
                   arrow = arrow(length = unit(0.07, "inches"), type = "closed"), 
                   alpha = 0.5) 
  } else {
    p_base
  }
  
}