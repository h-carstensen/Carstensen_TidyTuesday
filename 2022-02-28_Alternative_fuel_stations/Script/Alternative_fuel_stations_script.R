####### TidyTuesday Week of 2022-02-28 - Alternative Fuel Stations ######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-03-01

### load libraries ###
library(tidyverse)
library(here)

### read in data ###
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') #TidyTuesday data
mapdata <- map_data("state", region = "california") #Map data for California

### Data Analysis ###
CA_stations <- stations %>%  #Making a subset of data called CA_stations
  filter(STATE == "CA", #Filtering to just data from California
         FUEL_TYPE_CODE == "HY") %>%  #Filtering just to hydrogen fuel stations
  select("LATITUDE", "LONGITUDE", "FUEL_TYPE_CODE", "OWNER_TYPE_CODE") #Selecting data columns to keep in new dataframe

ggplot()+  #Starting a plot
  geom_polygon(data = mapdata, #Using polygon geometry to make California map from map data
               aes(x = long, #Mapping x axis to longitude
                   y = lat, #Mapping y axis to latitude
                   group = group), #Mapping group aesthetic to group column in map data
               fill = "lightgray")+ #Setting fill color for California polygon to light gray
  geom_point(data = CA_stations, #Using point geom to plot points from CA_stations onto map
             alpha = 0.4, #Setting alpha to make the points transparent
             aes(x = LONGITUDE,  #Mapping x axis to longitude
                 y = LATITUDE,  #Mapping y axis to latitude
                 color = OWNER_TYPE_CODE), #Mapping color to owner of fuel station
             size = 1.2)+ #Setting size of points
  theme_dark() + #Using dark theme for plot
  labs(x = "Longitude", #Adding x-axis label
       y = "Latitude", #Adding y-axis label
       color = "Station Owner", #Changing legend title
       title = "Hydrogen Fuel Station Locations in California", #Adding title
       caption = "TidyTuesday, week of 2022-02-28, Alternative Fuel Stations") + #Adding caption with data source
  guides(alpha = "none") + #Removing legend for alpha
  scale_color_viridis_d(labels = c("Local Government", "Privately Owned", "State Government")) #Changing point colors to viridis and changing legend labels

ggsave(here("2022-02-28_Alternative_fuel_stations", "Output", "Alternative_fuel.png"),  # Saving plot as .png
       width = 5, height = 4)  # Width and height of output
