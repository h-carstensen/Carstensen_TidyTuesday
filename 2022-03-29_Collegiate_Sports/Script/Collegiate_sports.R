####### TidyTuesday Week of 2022-03-29 - Collegiate Sports ######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-03-31

#### Load Libraries ####
library(tidyverse)
library(here)

#### Load Data ####
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

#### Data Analysis ####

expend <- sports %>% 
  group_by(sports) %>%   #Group data by sport
  summarise(total_exp = sum(total_exp_menwomen, na.rm = TRUE),  #Add up total expenditure per sport
            total_exp_men = sum(exp_men, na.rm = TRUE),  #Add up total expenditure per sport for men
            total_exp_women = sum(exp_women, na.rm = TRUE)) %>% #Add up total expenditure per sport for women
  na.omit() %>%   #Remove NAs
  mutate(proportion_men = (total_exp_men/total_exp)*-1, #Calculate proportion of total expenditure went to men, and make values negative
         proportion_women = total_exp_women/total_exp) %>% #Calculate proportion of total expenditure went to women
  na.omit() %>%  #Remove NAs
  mutate(num = 1:nrow(.),  #Create a new column with numbers
         sports = fct_reorder(sports, proportion_women)) #Reorder the factors by proportion of women

expend %>% 
  ggplot() +  #Start plot
  geom_col(aes(x = sports,  #Make column geometry with sports mapped to x-axis
               y = proportion_women),  #Map proportion of expenditure to women to y-axis
           fill = "#AA180E") +  #Fill color for columns
  geom_col(aes(x = sports,  #Make another column geometry with sports mapped to x-axis
               y = proportion_men),  #Map proportion of expenditure to men to y-axis
           fill = "#0041CC") +  #Fill color for columns
  geom_hline(yintercept = c(-0.75, -0.5, -0.25, 0.25, 0.5, 0.75), #Create y-intercept lines
             lty = "dotted")+  #Make the lines dotted
  scale_y_continuous(labels = c(1, 0.5, 0, 0.5, 1),  #Make y-axis labels on only these values
                     limits = c(-1, 1)) +  #Set limits of y-axis
  geom_text(aes(label = sports, x = sports, y = 0),  #Use geom_text to make new labels for the sport names
            color = "white", #Change text color
            angle = 90,  #Make them perpendicular to the columns
            size = 3) +  #Change text size
  theme(plot.background = element_rect(fill = "#0C335A"),  #Change plot background color
        panel.background = element_rect(fill = "#0C335A"),  #Change panel background color
        axis.ticks = element_blank(),  #Remove axis ticks
        axis.title = element_blank(),  #Remove axis title
        axis.text.x = element_blank(),  #Remove x-axis text
        axis.text.y = element_text(color = "white"), #Change color of y-axis text
        panel.grid = element_blank(),  #Remove panel grid
        plot.title = element_text(color = "white"),  #Change color of title text
        plot.caption = element_text(color = "white")) +  #Change color of caption text
  annotate(geom = "rect",   #Add a rectangle
           xmin = "Track and Field, X-Country", xmax = "Archery",  #Define boundaries of rectangle
           ymin = 0.85, ymax = 0.95,  #Define boundaries of rectangle
           fill = "black",  #Rectangle fill color
           alpha = 0.3) +  #Make it translucent
  annotate(geom = "text", x = "Skiing", y = 0.9, hjust = 0,  #Add a text label and choose location
           label = "More expenditure on women",  #Text 
           color = "#F04E42",  #Text color
           size = 3.5) +  #Text size
  annotate(geom = "rect",   #Add a rectangle
           xmin = "Track and Field, X-Country", xmax = "Swimming", #Define boundaries of rectangle
           ymin = -0.85, ymax = -0.95,  #Define boundaries of rectangle
           fill = "black",  #Rectangle fill color
           alpha = 0.3) +  #Make it translucent
  annotate(geom = "text", x = "Skiing", y = -0.9, hjust = 0,  #Add a text label and choose location
           label = "More expenditure on men",  #Text 
           color = "#85ABFF",  #Text color
           size = 3.5) +  #Text size
  labs(title = "Proportion of Expenditures for Men and Women in Collegiate Sports",  #Add plot title
       caption = "Data from Equity in Athletics Data Analysis via TidyTuesday")  #Add plot caption with data source

ggsave(here("2022-03-29_Collegiate_Sports", "Output", "Sports.png"),
       height = 5, width = 7.5)




