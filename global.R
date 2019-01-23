# Load packages
library(DT)
library(highcharter)
library(leaflet)
library(lubridate)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# Colors for Confirmed / Presumptive / No evidence:
cols <- c('#ca0020', '#f4a582', '#92c5de')


cols_elisa_ns1 <- c("Negative" = "blue", 
                    "not done" = "#f0f0f0", 
                    "not done (PCR+)" = "#bdbdbd", 
                    "not done (no sample)" = "#636363", 
                    "Positive" = "#e31a1c")

cols_elisa_igm <- c("Negative" = "blue", 
                    "not done" = "#f0f0f0", 
                    "not done (PCR+)" = "#bdbdbd", 
                    "not done (no sample)" = "#636363", 
                    "not done (NS1+)" = "#252525",
                    "Positive" = "#e31a1c")


cols_rdt <- c("Negative" = "blue", "Positive" = "#e31a1c")

cols_pcr_result <- c("Negative" = "blue", 
              "not done" = "#f0f0f0", 
              "not done (no sample)" = "#636363", 
              "Positive" = "#e31a1c",
              "equivocal" = "green")

cols_pcr_serotype <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")