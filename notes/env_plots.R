library(tidyverse)
site_info <- read_csv("app/data/site_info.csv")

# Read in data and join with site info
data <- 
  read_csv("app/data/gsi_living_lab_data.csv") |> 
  right_join(site_info) |> 
  mutate(datetime = with_tz(datetime, "America/Phoenix")) |> 
  filter(datetime > (max(datetime, na.rm = TRUE) - hours(36)))

# gsi_plot_paw <- function(data) {
  
plot_df <- data |> 
  filter(sensor_model == "TEROS12")
#TODO average across one or more variables: basin, depth_height_m, location


ggplot(plot_df, aes(x = datetime, y = paw.value, color = site)) +
  geom_line(alpha = 0.75, linewidth = 1.5, na.rm = TRUE) +
  facet_grid(depth_height_m~basin, labeller = label_both) +
  #TODO what are units for PAW?
  labs(x = "", y = "Plant Available Water (UNITS)") +
  scale_x_datetime(expand = c(0,0)) +
  scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
  guides(color = "none", fill = "none") + #turn off legend
  theme(axis.title.x = element_blank())

# }


data <- 
  read_csv("app/data/gsi_living_lab_data.csv") |> 
  right_join(site_info) |> 
  mutate(datetime = with_tz(datetime, "America/Phoenix")) |> 
  filter(datetime >= "2023-08-07", datetime <= "2023-08-09")
  # filter(datetime >= "2024-01-06", datetime < "2024-01-14")

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
ggplot(plot_df, aes(x = datetime, color = site, fill = site)) +
  geom_line(aes(y = air_temperature.value), alpha = 0.5) + #or line as "real" temp
  geom_segment(aes(y = air_temperature.value, yend = temp_adj, xend = datetime),
               arrow = arrow(length = unit(0.07, "inches"), type = "closed"), 
               alpha = 0.5) +
  # geom_point(aes(shape = adjustment, y = air_temperature_adj.value), alpha = 0.7) +
  # scale_shape_manual(values = c("hotter" = 24, "colder" = 25)) +
  scale_x_datetime(expand = c(0,0)) +
  scale_color_manual(values = gsi_site_colors, aesthetics = c("fill", "color")) + #defined in 0-theme_gsi.R
  labs(y = "Temperature (ºC)") +
  theme(axis.title.x = element_blank(), legend.position = "none")


}

