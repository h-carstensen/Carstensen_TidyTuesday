####### TidyTuesday Week of 2022-03-14 - R Vignettes######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-03-15


#### Load Libraries ####
library(tidyverse)
library(here)
library(lubridate)
library(ggthemes)

#### Load Data ####
bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

#### Data Analysis ####

cran2 <- cran %>% #Make a new data frame
  mutate(datetime = ymd_hms(date),  #Convert date to datetime using lubridate
         year = year(datetime)) %>%   #Extract year into new column
  group_by(year) %>%   #Group by year
  summarise(across(c(rnw:rmd), sum)) %>%  #Calculate sum of rnw or rmd published per year
  filter(year %in% c("2011", "2012", "2013", "2014", "2015", "2016", "2017", 
                     "2018", "2019", "2020", "2021")) %>%  #Filtering for years where there were publications
  rename(RMarkdown = "rmd",  #Rename rmd column
         Rnw = "rnw") %>%  #Rename rnw column 
  pivot_longer(cols = Rnw:RMarkdown,  #Pivot longer by these two colunms
               names_to = "Variables",  #Make new column called variables with old column names
               values_to = "Values")   #Make new column called values

colors <- c("#037971", "#eba0ba")  #Create a custom color palette

cran2 %>%   #Start with cran2 data frame
  ggplot(aes(x = year,  #Start ggplot with year mapped to x-axis
             y = Values,  #Map Values to y-axis
             fill = Variables)) +  #Map fill to variables to create grouped columns for rmd and rnw
  geom_col(stat = "identity",  #Telling ggplot to skip aggregation and use values provided
           position = position_dodge(),  #Using position dodge so the grouped columns don't overlap
           color = "black") +  #Outlining bars
  scale_y_continuous(breaks = scales::breaks_width(1000)) +  #Making y-axis breaks every 1000 vignettes
  scale_x_continuous(breaks = scales::breaks_width(1)) +  #Making x-axis break every year
  labs(title = "R Package Vignettes Published Per Year",  #Adding title
       caption = "Data from Robert Flight via TidyTuesday, week of 2022-03-14", #Adding caption with data source
       fill = "Vignette Type") + #Changing legend title
  theme_fivethirtyeight() +  #Changing theme to fivethirtyeight from ggthemes
  scale_fill_manual(values = colors)  #Applying the custom color palette

#Saving output

ggsave(here("2022-03-14_R_vignettes", "Output", "R_Vignettes.png"),
       width = 6, height = 5)



