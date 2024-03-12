library(tidyverse)
library(ggtext)

gsi_plot_et <- function(data_et) {
  p <- 
    ggplot(data_et, aes(x = datetime, y = ETo.value, color = site)) +
    geom_line(alpha = 0.75, linewidth = 1) +
    labs(x = "", y = "ET<sub>0</sub> (mm)") +
    scale_x_date(expand = c(0,0)) +
    scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
    guides(color = "none", fill = "none") + #turn off legend
    theme(axis.title.x = element_blank(), axis.title.y = element_markdown())
  
  if (nrow(data_et) == 0) {
    p + annotate("text", x = I(0.5), y = I(0.5), label = "No data available for these dates")
  } else {
    p
  }
}
