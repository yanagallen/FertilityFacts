###########################################################################
# Script: extract_acs.R
# Author: Gustavo Luchesi
# Last Updated: 8/27/2024
# Description: Extracts ACS PUMS dat file from raw and creates intermediate datasets
# based on the American Community Survey PUMS

# Input: usa_00001.dat (ACS PUMS 2000 - 2022 1 year dat file from raw)

# Output: acs_percent_pregnant_cohorts.csv
###########################################################################

# Installing required packages
packages <- c("tidyverse", "ipumsr", "data.table")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Loading extract into R and converting to data.table format
ddi <- read_ipums_ddi("raw/usa_00001.xml")
data <- read_ipums_micro(ddi)
data <- as.data.table(data)

# Creating a dataset for birth in last 12 months
filtered_data <- data[FERTYR != 0, .(YEAR, AGE, SEX, FERTYR, PERWT, EXPWTP)]
rm(data)

# Summarizing fertility by age across cohorts
fertility_by_age_ex2020 <- filtered_data[YEAR != 2020, .(N = sum(PERWT)), by = .(YEAR, AGE, FERTYR)] %>% as.data.frame()
fertility_by_age_2020 <- filtered_data[YEAR == 2020, .(N = sum(EXPWTP)), by = .(YEAR, AGE, FERTYR)] %>% as.data.frame()

fertility_by_age <- rbind(fertility_by_age_ex2020, fertility_by_age_2020)

percent_pregnant <- fertility_by_age %>% 
  
  group_by(AGE, YEAR) %>% 
  mutate(TOTAL = sum(N)) %>% 
  ungroup() %>% 
  
  mutate(PERCENT_PREGNANT = N / TOTAL) %>%
  filter(FERTYR == 2) %>% 
  select(-c(FERTYR, TOTAL, N)) %>% 
  
  arrange(YEAR, AGE)

# Saving intermediate dataset
path = "refined/acs_percent_pregnant_cohorts.csv"
write.csv(percent_pregnant, file = path, row.names = FALSE)
