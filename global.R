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

dengue <- dengue_data$dengue

d <- dengue %>%
  filter(dengue_virus %in% c("Presumptive", "Confirmed"))

d2 <- d %>% mutate(month = month(collection_date, label = TRUE)) %>% group_by(collection_year, month) %>% summarise(total = n())