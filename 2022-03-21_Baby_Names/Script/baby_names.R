####### TidyTuesday Week of 2022-03-21 - Baby Names ######
## Script and plot created for TidyTuesday
## Created by: Heather Carstensen
## Created on: 2022-03-21


#### Load Libraries ####

library(tidyverse)
library(here)
library(wordcloud2)
library(webshot)
library(htmlwidgets)
library(magick)


#### Load Data ####

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


#### Data Analysis ####

babynames_2 <-  babynames %>%   #Make a new data frame
  filter(year == "1994",  #Filter to just year 1994
         n > 1000,   #Filter to names that were used more than 1000 times
         sex == "F") %>%   #Filter to just girls
  select("name","n")  #Select columns


colors <- c("#A0DDFF", "#C1CEFE", "#C4F1BE", "#FFBFA0", "#EF798A", 
            "#FFFEAD", "#F9C7E0", "#13CDC4", "#F9398C", "#758BFD") #Create a custom color palette


cloud <- wordcloud2(data=babynames_2, #Make a wordcloud with wordcloud2 package
           size=0.5,   #Choose size of word cloud
           color=rep_len((colors), nrow(babynames_2)), #applying the custom color palette
           backgroundColor = "black",  #Making background black
           shape = 'circle')  #Making word could shape a circle

saveWidget(cloud, "cloud.html", selfcontained = F)  #Save the widget html

webshot("cloud.html","cloud.png", #Saving the widget output as a png
        delay=5, #Capture after a delay of 5 seconds
        vwidth = 900, vheight = 900)  #Set the width and height of png output


wordcloud <- image_read(here("2022-03-21_Baby_Names", "Output", "cloud.png"))  #Read in wordcould.png

baby_names_plot <- image_annotate(wordcloud, #Use magick package to annotate wordcloud.png
                                  "Most Popular Baby Girl Names in 1994 (Data via TidyTuesday)", #Add a title
                                  size = 30,  #Select font size
                                  color = "white",   #Select font color
                                  boxcolor = "black")  #Select text background color

image_write(baby_names_plot, path = "baby_names_plot.png", format = "png")  #Save as a png file
