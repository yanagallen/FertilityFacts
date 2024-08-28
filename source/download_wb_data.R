###########################################################################
# Script: download_wb_data.R
# Author: Gustavo Luchesi
# Last Updated: 8/28/2024
# Description: Download and extract World Bank fertility data to refined folder

# Input: World Bank Fertility raw data from WB API

# Output: wb_tfr_1960_2022.csv
###########################################################################

# Installing required packages
packages <- c("tidyverse", "wbstats")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Downloading Total Fertility Rates from World Bank API
series <- "SP.DYN.TFRT.IN" 
start <- 1960
end <- 2022

tfr_1960_2022 <- wb_data(indicator = series, start_date = start, end_date = end)

# Selecting and renaming variables
cleaned_tfr_1960_2022 <- tfr_1960_2022 %>% 
  
  select(iso3c, country, date, SP.DYN.TFRT.IN) %>% 
  
  rename(tfr = SP.DYN.TFRT.IN,
         country_code = iso3c,
         year = date) %>% 
  
  arrange(country, year)

# Saving TFR dataset
path = "refined/wb_tfr_1960_2022.csv"
write.csv(cleaned_tfr_1960_2022, file = path, row.names = FALSE)
