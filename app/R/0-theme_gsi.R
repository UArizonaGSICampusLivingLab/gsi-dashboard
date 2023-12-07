library(ggplot2)

#define custom ggplot2 theme
theme_gsi <- function() {
  theme_linedraw() +
    theme(
      # panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#set it as the default theme for all plots.
theme_set(theme_gsi())
