# Load packages
library(DT)
library(leaflet)
library(lubridate)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# Load data -- TODO: remove in production
load("./www/Dengue_Data_Latest.RData")


d <- dengue_data$dengue %>%
  mutate(month = month(collection_date, label = TRUE)) %>%
  filter(dengue_virus %in% c("Presumptive", "Confirmed"))

d2 <- d  %>% 
  group_by(collection_year, month) %>% 
  summarise(total = n())