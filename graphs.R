library(tidyverse)

metadata <- read.csv("metadata.csv")
data <- read.csv("gsi_living_lab_data.csv")|>
  mutate(datetime = ymd_hms(datetime))

fulldata <- merge(metadata, data, by = c("device_sn", "sensor"))

#select a site 
  #  select time frame to see
   #   select a single sensor 
# select variables to see plotted inside the geomline function 
input_site <- "Old Main"
input_mindate <- as.Date("2023-9-01")
input_maxdate <- as.Date("2023-10-31 00:00:00")


sitetimesensordata <- fulldata |>
  filter(site == input_site) |>
  filter(datetime >= input_mindate & datetime <= input_maxdate)


#ggplot2 geomline function is not able to read the dates sequentially when there's just one variable, how to  make these consecutive for r to read? 
# right now the datetime column is in character format so that could be an issue 
# datetiume column is not ordered correctly and is all over the place 
# is there a way to remove the T and the z in the datetime column and put a space between the 2023-10-11 and 00:00:00

soil_data <- sitetimesensordata |> 
  filter(str_starts(sensor, "T")) #all soil sensors start with "T"

soil_temp_mean <- soil_data |> 
  dplyr::summarise(
    soil_temp_mean = mean(soil_temperature.value),
    .by = c(site, datetime, depth_height_m)
  ) |> 
  mutate(depth_m = as.factor(depth_height_m))

ggplot(data = soil_data, aes(x = datetime)) +
  # geom_line(aes(y = soil_temperature.value, color = sensor)) +
  geom_line(data = soil_temp_mean, aes(y = soil_temp_mean, color = depth_m)) +
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  labs(color = "Depth (m)") #change x, y, title, etc.
 
# geom_col() #for making column charts (e.g. rainfall)
    

ggplot(data = soil_data, aes(x = datetime)) +   
  geom_line(aes(y = water_content.value, color = sensor))    
  
ec_plot <- 
  ggplot(data = soil_data, aes(x = datetime)) +
  geom_line(aes(y = bulk_ec.value, color = sensor))
ec_plot

# for making interactive plots
library(plotly)

ggplotly(ec_plot)
