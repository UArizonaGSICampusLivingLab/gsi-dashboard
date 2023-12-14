library(ggplot2)

# Colors from UA branding guidelines https://marcom.arizona.edu/brand-guidelines/colors
# Primary colors
az_red <- "#AB0520" #used in navbars
az_blue <- "#0C234B"

# Neutrals
warm_gray <- "#F4EDE5"
cool_gray <- "#E2E9EB"

# Complimentary
midnight <- "#001C48"
azurite <- "#1E5288" #used in banners
oasis <- "#378DBD"
chili <- "#8B0015"

# Highlight/Accent
bloom <- "#EF4056"
sky <- "#81D3EB"
leaf <- "#70B865"
river <- "#007D84"
mesa <- "#A95C42"

#define custom ggplot2 theme
theme_gsi <- function() {
  theme_linedraw(base_size = 14) + #change font size of all text
    theme(
      # panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = az_red, color = az_red)
    )
}

#set it as the default theme for all plots.
theme_set(theme_gsi())

#defines colors for sites for use in scale_color_manual() etc.
gsi_site_colors <- 
  c("Old Main" = "#AB0520", "Gould Simpson" = "#70B865", "Physics and Atmospheric Sciences" = "#1E5288")
