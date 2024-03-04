#Graphs!! 
library(dplyr)
library(ggplot2)
data <- read.csv("gsi_living_lab_data.csv")
View(data)

#Data by logger 
graphdata <- fulldata |>
  filter(site == "Old Main")|> #choose loggers
   filter(water_content.value != "NA") 
    # filter out na variables 

View(graphdata)



ggplot(data = graphdata, aes(x=datetime))+
  geom_line(aes(y = water_content.value, color = sensor))
  
summary_logger <- fulldata |>
  group_by(device_sn, )|>
  summarize(water_content_mean = mean(water_content.value, na.rm = TRUE), 
  water_content_max = max(water_content.value, na.rm = TRUE),
  water_content_min = min(water_content.value, na.rm = TRUE),
  water_content_sd = sd(water_content.value, na.rm = TRUE))


### Combine metadata with zentra cloud data 
metadata <- read.csv("metadata.csv")
# merge order of importance: device_sn, then sensor 
fulldata <- merge(metadata, data, by = c("device_sn", "sensor"))  # date column gets out of order, need to re order

# add device sn first as it is the most overlapping variable, 

View(fulldata)
summary_site <- fulldata |>
  group_by(site, basin, depth_height_m)|>
  summarize(water_content_mean = mean(water_content.value, na.rm = TRUE), 
            water_content_max = max(water_content.value, na.rm = TRUE),
            water_content_min = min(water_content.value, na.rm = TRUE),
            water_content_sd = sd(water_content.value, na.rm = TRUE))

summary_site <- fulldata |>
  group_by(site, basin, depth_height_m)|>
  summarize(water_content_mean = mean(water_content.value, na.rm = TRUE), 
            water_content_max = max(water_content.value, na.rm = TRUE),
            water_content_min = min(water_content.value, na.rm = TRUE),
            water_content_sd = sd(water_content.value, na.rm = TRUE))

  

