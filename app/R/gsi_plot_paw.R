gsi_plot_paw <- function(data) {
  
  plot_df <- data |> 
    filter(sensor_model == "TEROS12") |> 
    #summarize sensors at same site, basin, and depth
    dplyr::summarize(
      paw.value = mean(paw.value),
      .by = c(site, basin, datetime)
    ) |> 
    #recode basin to "basin" and "non-basin"
    mutate(basin = case_when(
      basin == "n" ~ "non-basin",
      basin == "y" ~ "basin",
      .default = NA
    ))
  
  ggplot(plot_df, aes(x = datetime, y = paw.value, color = site, linetype = basin)) +
    geom_line(alpha = 0.75, linewidth = 1, na.rm = TRUE) +
    facet_wrap(~site) +
    labs(x = "", y = "Plant Avail. Water", linetype = "") +
    scale_x_datetime(expand = c(0,0)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
}