####### TidyTuesday Week of 2022-02-21 - Freedom in the World ######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-02-21


#### load libraries ####
library(tidyverse)  #Loading tidyverse
library(here)  #Loading here

#### read in data ####
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

#### data analysis ####
freedom_long <- freedom %>%   # Making object called freedom_long
  rename("Civil Liberties" = CL, "Political Rights" = PR) %>%   # Renaming CL and PR with their full names, Civil Liberties and Political Rights
  filter(country %in% c("United States of America", "Canada", "Mexico")) %>%   # Filtering to three countries
  pivot_longer(cols = 'Civil Liberties':'Political Rights',   # Pivoting the Civil Liberties and Political Rights columns to long format
               names_to = "Variables",   # Namking new Variables column
               values_to = "Values")   # Naming new Values column
  
  
freedom_long %>%   # Using freedom_long for a plot
  ggplot(mapping = aes(x = year,  # starting a plot, mapping year to the x-axis
                       y = Values,   # mapping Values to the y-axis
                       color = country,   # mapping color to country
                       linetype = country)) +   # mapping line type to country, so overlapping lines will be visible
  geom_line(size = 1.5,  # Increasing the line size
            alpha = 0.5)+   # Chaning alpha to make lines transparent
  facet_wrap(~Variables) +   # Facet wrapping the plots by Variables, so there is a plot each for Civil Liberties and Political Rights
  labs(x = "Year",   # Changing x-axis label from year to Year
       color = "Country",  # Changing legend title to Country
       linetype = "Country",  # Changing legend title to Country
       title = "Civil Liberties and Political Rights Scores over Time in \nCanada, Mexico, and United States of America",  # Adding second line of title
       caption = "TidyTuesday, week of 2022-02-21, Freedom in the World") +  # Adding caption with data source
  theme(plot.title = element_text(hjust = 0.5),  # Centering title
        panel.background = element_rect(color = "black",  # Adding black border line around each plot
                                        fill = "gray94"))  # Changing plot background color

ggsave(here("2022-02-21_freedom_in_the_world", "Output", "Freedom_plot.png"),  # Saving plot as .png
       width = 9, height = 4)  # Width and height of output
  
