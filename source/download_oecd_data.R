###########################################################################
# Script: download_oecd_data.R
# Author: Gustavo Luchesi
# Last Updated: 8/28/2024
# Description: Download and extract OECD spending data to refined folder

# Input: Social Spending raw data from OECD API

# Output: oecd_spending_data.csv
###########################################################################

# Installing required packages
packages <- c("tidyverse", "devtools")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Installing development version of OECD package
github_packages <- c("OECD")

to_install_git <- github_packages[!(github_packages %in% installed.packages()[,"Package"])]

if(length(to_install_git) > 0) install_github(paste0("expersso/", to_install_git))

lapply(github_packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Downloading Childcare Spending data from OECD
dataset <- "OECD.ELS.SPD,DSD_SOCX_AGG@DF_PUB_FAM,1.0"
names <- c("2015usd_ps", "pct_gdp")
filters <- c(".A.SOCX.USD_PPP_PS.._T.TP51.Q", ".A.SOCX.PT_B1GQ.._T.TP51._Z")

filter_2015usd <- ".A.SOCX.USD_PPP_PS.._T.TP51.Q"
filter_pct_gdp <- ".A.SOCX.PT_B1GQ.._T.TP51._Z"

raw_2015usd <- get_dataset(dataset, filter_2015usd)
raw_pct_gdp <- get_dataset(dataset, filter_pct_gdp)

# Cleaning the datasets
clean_oecd_socx <- function(raw_data) {
  
  raw_data %>% 
    
    select(ObsValue, REF_AREA, TIME_PERIOD) %>% 
    
    rename(country_code = REF_AREA,
           year = TIME_PERIOD) %>% 
    
    mutate(ObsValue = as.numeric(ObsValue)) %>% 
    
    complete(year, country_code) %>% 
    
    arrange(country_code, year) %>% 
    
    select(country_code, year, ObsValue)
  
}

# Merging all extracted series

clean_2015usd <- clean_oecd_socx(raw_2015usd)
clean_pct_gdp <- clean_oecd_socx(raw_pct_gdp)

cleaned_data <- clean_2015usd %>% 
  
  left_join(clean_pct_gdp, by = c("country_code", "year")) %>% 
  
  rename(pc_family_spending_2015usd = ObsValue.x,
         family_spending_to_gdp = ObsValue.y)

# Saving final dataset
path = "refined/oecd_spending_data.csv"
write.csv(cleaned_data, file = path, row.names = FALSE)

