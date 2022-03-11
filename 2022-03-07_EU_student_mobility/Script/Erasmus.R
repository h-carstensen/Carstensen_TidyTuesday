####### TidyTuesday Week of 2022-03-07 - EU Student Mobility ######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-03-08


### Load Libraries ###
library(tidyverse)
library(here)
library(circlize)
library(ggplotify)
library(cowplot)


### Load Data ###
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')


### Data Analysis ###
erasmus_sum <- erasmus %>%   #Making new data frame
  filter(sending_country_code %in% c("DK", "FI", "NO", "SE", "BE", "FR", "DE", "LU", "NL"),  #Filtering to a few Northwestern European countries
         receiving_country_code %in% c("DK", "FI", "NO", "SE", "BE", "FR", "DE", "LU", "NL"),
         sending_country_code != receiving_country_code) %>%  #Filtering out students who moved within their own country
  group_by(sending_country_code, receiving_country_code) %>%  #Grouped by sending and receiving countries
  summarise(n=n()) #Summarizing by number of cases

#Recoding country codes to full country names

erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "DK" = "Denmark")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "FI" = "Finland")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "NO" = "Norway")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "SE" = "Sweden")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "BE" = "Belgium")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "FR" = "France")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "DE" = "Germany")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "LU" = "Luxembourg")
erasmus_sum$sending_country_code <- recode(erasmus_sum$sending_country_code, "NL" = "Netherlands")

erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "DK" = "Denmark")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "FI" = "Finland")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "NO" = "Norway")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "SE" = "Sweden")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "BE" = "Belgium")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "FR" = "France")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "DE" = "Germany")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "LU" = "Luxembourg")
erasmus_sum$receiving_country_code <- recode(erasmus_sum$receiving_country_code, "NL" = "Netherlands")

#Chosing a custom color palette with hex color codes
colors <- c("#49A078", "#EDD892", "#BD1E1E", "#1098F7", "#2C497F", "#2D2A32", "#770026", "#FFC0C0", "#EC7D10")

chordDiagram(erasmus_sum, grid.col = colors)  #Making a chord diagram with the summary data, using the color palette

plot <- recordPlot()  #Saving this plot as an object to layer labels onto it with ggplot

as.ggplot(ggdraw(plot)) +  #Making the plot into a ggplot object
  labs(title = "Erasmus Student Mobility in \nNorthwestern Europe",  #Adding title
       caption = "TidyTuesday week of 2022-03-07. Data from Data.Europa") +  #Adding caption
  theme(plot.title = element_text(hjust = 0.5))  #Centering title


ggsave(here("2022-03-07_EU_student_mobility", "Output", "Erasmus.png"))  #Saving plot

       