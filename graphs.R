library(ggplot2)
library(dplyr)

read.csv("metadata.csv")
read.csv("gsi_living_lab_data.csv")

fulldata <- merge(metadata, data, by = c("device_sn", "sensor"))

#select a site 
  #  select time frame to see
   #   select a single sensor 
# select variables to see plotted inside the geomline function 


sitetimesensordata <- fulldata |>
  filter(site == "Old Main") |>
    filter(datetime >= "2023-10-01T00:00:00Z" & datetime <= "2023-10-31T00:00:00Z") |>
      filter(sensor == "T12-00118460")

sitetimesensordata[order(as.Date(sitetimesensordata$datetime, format="%Y-%M-%DT%H:%M:%SZ")),] # attempt to order data by datetime, did not work 

#ggplot2 geomline function is not able to read the dates sequentially when there's just one variable, how to  make these consecutive for r to read? 
# right now the datetime column is in character format so that could be an issue 
# datetiume column is not ordered correctly and is all over the place 
# is there a way to remove the T and the z in the datetime column and put a space between the 2023-10-11 and 00:00:00

ggplot(data = sitetimesensordata, aes(x = datetime))+
  geom_line(aes(y = soil_temperature.value, group = 1))   # this graph might be correct for some reason

ggplot(data = sitetimesensordata, aes(x = datetime))+   
  geom_line(aes(y = water_content.value, group = 1))   # this graph is incorrect 
  
ggplot(data = sitetimesensordata, aes(x = datetime))+
  geom_line(aes(y = bulk_ec.value, group = 1))    # this graph is incorrect 



