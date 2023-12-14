library(ggplot2)

#define custom ggplot2 theme
theme_gsi <- function() {
  theme_linedraw(base_size = 14) + #change font size of all text
    theme(
      # panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#AB0520")
    )
}

#set it as the default theme for all plots.
theme_set(theme_gsi())

#defines colors for sites for use in scale_color_manual() etc.
gsi_site_colors <- 
  c("Old Main" = "#AB0520", "Gould Simpson" = "#70B865", "Physics and Atmospheric Sciences" = "#1E5288")
