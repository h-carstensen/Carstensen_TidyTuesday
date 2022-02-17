####### DuBois Challenge 2022 code for TidyTuesday ######
## Created by: Heather Carstensen
## Created on: 2022-02-15


#### load libraries ####
library(tidyverse)  #Loading tidyverse
library(here)  #Loading here

#### read in data ####
data <- readr::read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge04/data.csv")

#### data analysis ####
ggplot(data = data,  
       mapping = aes(x = Year,  #Set x-axis to year
                     y = `Property Valuation`)) +  #Set y-axis to 'Property Valuation'
  geom_line(size = 2) +  #Choose line geometry and increase line thickness from default
  labs(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED \nBY GEORGIA NEGROS", #Title of original DuBois plot with \n to add new line
       caption = "Source: DuBois Data Challenge 2022 for TidyTuesday, Challenge04",  #Add caption
       y = "DOLLARS",  #Change y-axis title
       x = "YEAR") +  #Change x-axis title
  theme(text = element_text(family = "mono"),  #Change font to something closer to DuBois style
        plot.title = element_text(hjust = 0.5),  #Centered plot title
        panel.background = element_rect(fill = "antiquewhite2", #Changed plot background color
                                        color = "black"),  #Added a black border around the plot panel
        plot.background = element_rect(fill = "antiquewhite2"), #Changed panel background color
        panel.grid.minor = element_line(size = 0.1, #Reduced minor gridline thickness
                                        colour = "firebrick"),  #Changed minor gridline color
        panel.grid.major = element_line(size = 0.1, #Reduced major gridline thickness
                                        colour = "firebrick")) + #Changed major gridline color
  scale_y_continuous(labels = scales::comma,  #Removed scientific notation from y-axis scale
                     minor_breaks = scales::breaks_width(100000)) + #Added minor gridline at every $100,000
  scale_x_continuous(minor_breaks = scales::breaks_width(1)) +  #Added minor gridline at every year
  geom_text(aes(x = 1876, y = 500000, label = "KU-KLUXISM", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1875, y = 2250000, label = "POLITICAL UNREST", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1890, y = 500000, label = "FINANCIAL PANIC", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1895, y = 1500000, label = "LYNCHING", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1880, y = 4250000, label = "RISE OF THE", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1880, y = 4000000, label = "NEW INDUSTRIALISM", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1896, y = 3000000, label = "DISFRANCHISMENT", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1896, y = 2750000, label = "AND PROSCAPTIVE", family = "mono")) +  # Adding plot text labels to approx. location on original
  geom_text(aes(x = 1896, y = 2500000, label = "LAWS", family = "mono"))  # Adding plot text labels to approx. location on original

ggsave(here("dubois_data_portraits", "outputs", "DuBois_challenge04.png"),  # Saving plot as .png
       width = 6, height = 7.62)  # Changing width and height to approx. the original aspect ratio
  
