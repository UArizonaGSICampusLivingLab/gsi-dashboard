gsi_plot_paw <- function(data) {
  
  plot_df <- data |> 
    filter(sensor_model == "TEROS12") |> 
    #summarize sensors at same site, basin, and depth
    dplyr::summarize(
      paw.value = mean(paw.value),
      .by = c(site, basin, depth_height_m, datetime)
    ) |> 
    #recode depth to be "0.5m" instead of -0.5 (for example)
    mutate(depth = paste(-depth_height_m, "m")) |> 
    #recode basin to "basin" and "non-basin"
    mutate(basin = case_when(
      basin == "n" ~ "non-basin",
      basin == "y" ~ "basin",
      .default = NA
    ))
  
  ggplot(plot_df, aes(x = datetime, y = paw.value, color = site)) +
    geom_line(alpha = 0.75, linewidth = 1, na.rm = TRUE) +
    facet_grid(depth~basin) +
    #TODO what are units for PAW?
    labs(x = "", y = "Plant Avail. Water (UNITS??)") +
    scale_x_datetime(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") + #turn off legend
    theme(axis.title.x = element_blank())
}