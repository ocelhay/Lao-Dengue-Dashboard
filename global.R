# Load packages
library(DT)
library(leaflet)
library(lubridate)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# Load data -- TODO: remove in production
# load("./www/Dengue_Data_Latest.RData")
# load("./App/www/Dengue_Data_Latest.RData")

# data_available <- FALSE
# 
# d <- dengue_data$dengue %>%
#   filter(dengue_virus %in% c("Presumptive", "Confirmed"))
# 
# d2 <- d  %>% 
#   group_by(collection_year, collection_month) %>% 
#   summarise(total = n())

# min_collection_date <- min(d$collection_date)
# max_collection_date <- max(d$collection_date)
# 
# min_collection_date <- min(d$collection_date)
# max_collection_date <- max(d$collection_date)
