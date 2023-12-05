
# Load pacakges -----------------------------------------------------------
library(tidyverse)
library(patchwork)


# Colors ------------------------------------------------------------------

#UA official branding colors https://brand.arizona.edu/applying-the-brand/colors

az_blue <- "#0C234B"
az_red <- "#AB0520"

# Load data ---------------------------------------------------------------

gsi_raw <- read_csv("gsi_living_lab_data.csv")
site_info <- read_csv("metadata.csv")

gsi <- right_join(site_info, gsi_raw)

# Plot data ---------------------------------------------------------------
colnames(gsi)

gsi_filtered <-   
  gsi |> 
  filter(site == "Old Main") |> filter(str_starts(sensor, "ATM")) 


precip <- 
  ggplot(gsi_filtered, aes(x = datetime, y = precipitation.value)) +
  geom_col(color = az_blue, fill = az_blue) +
  theme_bw() +
  labs(y = "Precipitation (mm)", x = "")

precip

air_temp <-
  ggplot(gsi_filtered, aes(x = datetime, y = air_temperature.value)) +
  geom_line(color = az_red) +
  theme_bw() +
  labs(y = "Air Temperature (ºC)", x = "")

air_temp

# Combine plots with `patchwork` ------------------------------------------

#remove all redundant x-axis stuff from top plot
precip_slim <- 
  precip + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

precip_slim / air_temp + 
  #make second plot 3 times taller
  plot_layout(heights = c(1, 3))
