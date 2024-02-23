library(ggiraph)
library(tidyverse)

data_full <- read_csv("data/data_full_ex.csv")


data_vp <- data_full |> 
  filter(!is.na(vpd.value), !is.na(vapor_pressure.value)) |> 
  #simulate some filters
  filter(datetime > ymd("2024-01-01")) |> 
  filter(site_code %in% c("om", "pas")) |> 
  #create tooltip column
  mutate(tooltip_vp = glue::glue("{site} (VP)"), tooltip_vpd = glue::glue("{site} (VPD)"))

gg <-
  ggplot(data_vp, aes(x = datetime, color = site)) +
  geom_line_interactive(aes(y = vpd.value, linetype = "VPD", tooltip = tooltip_vpd, data_id = tooltip_vpd), alpha = 0.5) +
  geom_line_interactive(aes(y = vapor_pressure.value, linetype = "VP", tooltip = tooltip_vp, data_id = tooltip_vp), alpha = 0.5) +
  scale_x_datetime(expand = c(0,0)) +
  scale_color_manual(values = gsi_site_colors) +
  guides(color = "none") + #turn off legend
  theme(axis.title.x = element_blank(), legend.position = "top") +
  labs(y = "Vapor Pressure / VPD (kPa)")

girafe(ggobj = gg)
