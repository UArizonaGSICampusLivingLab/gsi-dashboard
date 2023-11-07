#Graphs!! 
library(dplyr)
library(ggplot2)
data <- read.csv("gsi_living_lab_data.csv")
View(data)

#Data by logger 
om <- data |>
  filter(device_sn == "z6-19484" |device_sn == "z6-19485")|> #choose loggers
   filter(water_content.value != "NA") |> # filter out na variables 


ggplot(data = om, aes(x=datetime))+
  geom_line(aes(y = water_content.value, color = sensor))
  
summary_logger <- data |>
  group_by(device_sn, )|>
  summarize(water_content_mean = mean(water_content.value, na.rm = TRUE), 
  water_content_max = max(water_content.value, na.rm = TRUE),
  water_content_min = min(water_content.value, na.rm = TRUE),
  water_content_sd = sd(water_content.value, na.rm = TRUE))



