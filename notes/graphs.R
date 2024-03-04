library(tidyverse)
library(patchwork)
#read in data
metadata <- read.csv("metadata.csv")
data <- read.csv("gsi_living_lab_data.csv")|>
  mutate(datetime = ymd_hms(datetime))
#merge metadata and data
fulldata <- merge(metadata, data, by = c("device_sn", "sensor"))

#filter data
input_site <- "Old Main"
input_mindate <- as.Date("2023-9-01")
input_maxdate <- as.Date("2023-10-31 00:00:00") ##ask about pulling the latest date in the data here by default 
sitetimedata <- fulldata |>
  filter(site == input_site) |>
  filter(datetime >= input_mindate & datetime <= input_maxdate)


soil_data <- sitetimedata |> 
  filter(str_starts(sensor, "T"))   #all soil sensors start with "T"  #will need to filter out t21's later maybe

#####Atmospheric graphs 

atmos_data <- sitetimedata |> 
  filter(str_starts(sensor, "A")) #all atmospheric measurement sensors start with "A"

ggplot(data = atmos_data, aes(x = datetime)) +
  geom_line(aes(y = air_temperature.value), color = "lightblue")+
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Date", y = "Air Temperature (C)") #change x, y, title, etc.

ggplot(data = atmos_data, aes(x = datetime)) +
  geom_col(aes(y = precipitation.value), color = "coral")+
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Date", y = "Precipitation (cm)") #change x, y, title, etc.

ggplot(data = atmos_data, aes(x = datetime)) +
  geom_line(aes(y = vapor_pressure.value), color = "red")+
  geom_line(aes(y = vpd.value), color = "blue")+
 # geom_line(aes(y = atmospheric_pressure.value), color = "green")+
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Variables", 
                     values = c("red", "blue"), 
                     labels = c("Vapor Pressure", "VPD"))+
  labs(x = "Date", y = "Precipitation (cm)") #change x, y, title, etc.

p2 <- ggplot(data = atmos_data, aes(x = datetime)) +
  geom_line(aes(y = atmospheric_pressure.value), color = "green")+
  # geom_line(aes(y = atmospheric_pressure.value), color = "green")+
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Date", y = "(cm)") #change x, y, title, etc.

p2/p1












#create mean timeseries summary data for soil moisture differentiated by depth 
soil_temp_mean_depth <- soil_data |> 
  dplyr::summarise(
    soil_temp_mean = mean(soil_temperature.value),   #calculate mean soil temperature data   (this can be other summary functions) 
    .by = c(site, datetime, depth_height_m)        # Average all soil temperature values that share a similar site, datetime, and height
  ) |> 
  mutate(depth_m = as.factor(depth_height_m)) #creates a new column with depth as a factor for easier plotting

soil_temp_by_depth <- ggplot(data = soil_data, aes(x = datetime)) +
  geom_line(data = soil_temp_mean_depth, aes(y = soil_temp_mean, color = depth_m)) +
  geom_col(data = atmos_data, aes(y = max_precip_rate.value, width = NULL), color = "deepskyblue4")+
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(color = "Depth (m)", x = "Date", y = "Soil Temperature (C)") #change x, y, title, etc.

ggplot(data = soil_data, aes(x = datetime)) +
  geom_line(aes(y = soil_temperature.value, color = sensor)) +
  geom_col(data = atmos_data, aes(y = max_precip_rate.value, width = NULL), color = "deepskyblue4")+
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(color = "Depth (m)", x = "Date", y = "Soil Temperature (C)") #change x, y, title, etc.


test <- soil_data |>
  select(basin, water_content.value, datetime)
#facet graphs
#create this graph for an average of each site / edit the legend of this
# create mean timeseries data for soil moisture differentiated by basin 
soil_moisture_mean_basin <- soil_data |> 
  filter(water_content.value != "") |>
  dplyr::summarise(
    soil_moisture_mean = mean(water_content.value),   #calculate mean water content data   (this can be other summary functions) 
    .by = c(basin, site, datetime)) |>
  mutate(basin = as.factor(basin)) 
        # Average all soil temperature values that share a similar site, datetime, and height

#graph comparison of basin and non basin data 
ggplot(data = soil_data, aes(x = datetime)) +
  geom_line(data = soil_moisture_mean_basin, aes(y = soil_moisture_mean, color = basin)) +
  scale_x_datetime(date_breaks = "month") + #see help file section on date_labels for changing how dates are displayed
  theme_linedraw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(color = "Basin Yes/No", x = "Date", y = "Water Content (m³/m³)") #change x, y, title, etc.



## make a plot of othis constant time series average for values for each site, graph thos 
# work on the monthly average charts with the standard deviations shown 

    
# graph of differnt sensors from old main all on water content 
ggplot(data = soil_data, aes(x = datetime)) +   
  geom_line(aes(y = water_content.value, color = sensor))    


  
ec_plot <- 
  ggplot(data = soil_data, aes(x = datetime)) +
  geom_line(aes(y = bulk_ec.value, color = sensor))
ec_plot

# for making interactive plots
library(plotly)

# assign a plot to an object then plug it in to this function to make it interactive 
ggplotly(ec_plot)

ggplotly(soil_temp_by_depth)

# maybe we don't want to display the things like this I am unsure 


fulldata <- fulldata |>
  mutate(month = month(datetime, label = TRUE))

input_mindate <- as.Date("2023-9-01")
input_maxdate <- as.Date("2023-10-31 00:00:00")
# assign defaults to the function inputs with input_mindate = "YYYY-MM-DD" etc! yay
plot_monthly <- function(input_mindate, input_maxdate){
  

data_filtered <- fulldata |>
  filter(datetime >= as.Date(input_mindate) & datetime <= as.Date(input_maxdate))

ggplot(data = data_filtered, aes(x = month, y = air_temperature.value, color = site))+
         stat_summary(fun.data = mean_sdl, position = position_dodge(width = .7)) 
}

ggplot(data = data_filtered, aes(x = month, y = air_temperature.value, fill = site))+
    geom_boxplot()



#assign a plot to an object, then use patchwork package to align the figures and make a cool comparison. 

