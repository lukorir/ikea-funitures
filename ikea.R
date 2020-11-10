# TidyTuesday
# ikea data

# Working directory
setwd("C:/Users/lkorir/OneDrive - CGIAR/Documents/resakss market price data/Rural population age structure")

# Load libraries
library(tidyverse)
library(stringi)
library(wordcloud2)
library(extrafont)
library(magick)
rm(list = ls())
# ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')
# write.csv(ikea, "ikea.csv")

# Data
ikea <- read.csv("ikea.csv")
png("IKEA_names_cloud.png",width=600, height=400, res=96)
last_name_data <- ikea %>% select(designer) %>%
  filter(!is.na(designer)) %>% 
  mutate(name = designer,
         name = gsub('\\b\\w{1,2}\\s','', name)) %>% # Removes single letter befpore name
  # filter(str_detect(name, "IKEA")) %>% 
  mutate(name = gsub("\\ Sweden", "", name)) %>%  # Removes " Sweden" from "IKEA Sweden" 
  mutate(last_name = case_when(str_detect(name, "/") ~ name,
                            TRUE ~ stri_extract_last_words(name))) %>% # Removes "/" and replaces with " "
  mutate(last_name = str_split(last_name, "/")) %>% # Separate names of collaborators
  unnest(last_name) %>%  # PLace all names in a single column 
  #mutate(last_name = case_when(grepl("^[[:digit:]]", last_name) ~ "",
  separate(last_name, c("a", "b")) %>% 
  mutate(p = grepl("^[[:digit:]]", name)) %>% 
  filter(p == FALSE) %>% 
  mutate(b = case_when(is.na(b) ~ a,
                       TRUE ~ b)) %>% 
  select(last_name=b)


names <- data.frame(table(last_name_data$last_name)) %>% 
  mutate(Freq = as.numeric(Freq))


ikea <- wordcloud2(names, minSize = "IKEA", fontFamily = 'Brush Script MT',  size = .8, color = "random-light", backgroundColor = "black", shape = 'circle', )
ikea <- ikea(title("IKEA"))
             
IKEA_last_names_cloud <- image_read("IKEA_last_names_cloud.png")
image_annotate <- (IKEA_last_names_cloud, "Data : tidytuesday | Image : @lukorir", 
               size = 15, gravity = "southwest", color = "white",
               location = "+50+150")
IKEA_last_names_cloud <- image_read("IKEA_last_names_cloud_1.png")
image_annotate(IKEA_last_names_cloud, "Last names of IKEA \n designers", 
               size = 30, color = "white", location = "+50+20")

image_annotate(IKEA_last_names_cloud, "designers", 
               size = 30, color = "white", location = "+50+50")