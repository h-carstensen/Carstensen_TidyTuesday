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

cran2 <- cran %>% 
  mutate(datetime = ymd_hms(date),
         year = year(datetime)) %>% 
  group_by(year) %>% 
  summarise(across(c(rnw:rmd), sum)) %>% 
  filter(year %in% c("2011", "2012", "2013", "2014", "2015", "2016", "2017", 
                     "2018", "2019", "2020", "2021")) %>% 
  rename(RMarkdown = "rmd",
         Rnw = "rnw") %>% 
  pivot_longer(cols = Rnw:RMarkdown,
               names_to = "Variables",
               values_to = "Values") 

colors <- c("#037971", "#eba0ba")

cran2 %>% 
  ggplot(aes(x = year,
             y = Values,
             fill = Variables)) +
  geom_col(stat = "identity", position = position_dodge(), color = "black") +
  scale_y_continuous(breaks = scales::breaks_width(1000)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(title = "R package vignettes published per year",
       x = "Year",
       y = " ",
       caption = "Data from Robert Flight via TidyTuesday, week of 2022-03-14",
       fill = "Vignette Type") +
  theme_fivethirtyeight() +
  scale_fill_manual(values = colors)

ggsave(here("2022-03-14_R_vignettes", "Output", "R_Vignettes.png"),
       width = 6, height = 5)



