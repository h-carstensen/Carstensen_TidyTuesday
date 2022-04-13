####### TidyTuesday Week of 2022-04-12 - Indoor Air Pollution ######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-04-12

#### Load Libraries ####
library(tidyverse)
library(here)
library(gganimate)
library(PNWColors)

#### Read in data ####
indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')


#### Data analysis ####
pollution <- indoor_pollution %>% 
  filter(Entity %in% c("Africa", "Asia", "America", "Europe", "Oceania")) %>%  #Filtering for data in continents
  rename(Deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`, #Renaming deaths column
         Region = 'Entity')  #Renaming Entity column to Region


pal <- pnw_palette("Sunset",150,type="continuous") #Creating an object for color palette


pollution %>%  
  ggplot() +  #Starting a ggplot
  geom_col(aes(x = Year,  #using column geometry, mapping year to x-axis
           y = Deaths),  #mapping % deaths to y-axis
           fill = pal) +  #setting fill color for bars
  theme_bw() +  #Using bw theme
  transition_states(  #Starting animation by setting transitions tates
    Region,  #Setting transition state to region
    transition_length = 1,  #Setting transition length to 1
    state_length = 1) +  #Setting state length to 1
  ggtitle('Region: {closest_state}') +  #Setting the transitioning titles to the names of the Regions (continents)
  labs(subtitle = "Percentage of deaths attributed to indoor air pollution",  #Adding a subtitle
       caption = "Data from Our World in Data via TidyTuesday",  #Adding a caption with the data source
       y = "Percentage of Deaths",  #Changing the y-axis name
       x = "Year") +  #Adding an x-axis title
  anim_save(here("2022-04-12_Indoor_air_pollution","output","Air_pollution.gif"))  #Saving the animation as a .gif
  



