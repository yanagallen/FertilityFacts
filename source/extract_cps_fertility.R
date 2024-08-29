###########################################################################
# Script: extract_cps_fertility.R
# Author: Gustavo Luchesi
# Last Updated: 8/29/2024
# Description: Extracts CPS dat file from raw and creates intermediate datasets
# based on the Fertility Supplement of the CPS

# Input: cps_00002.dat

# Output: cps_women_1980_2022.csv
###########################################################################

# Installing required packages
packages <- c("tidyverse", "ipumsr")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Loading extract into R
ddi <- read_ipums_ddi("raw/cps_00002.xml")
data <- read_ipums_micro(ddi)

names(data) <- tolower(names(data))

# Generating a dataset for women in the 1980-2022 period
data_women_1980_2022 <- data %>% 
  
  filter(year %in% c(1980, 1990, 1995, 2012, 2014, 2016, 2018, 2020, 2022),
         sex == 2) %>% 
  
  mutate(frage = ifelse(frage1 %in% c(0, 999), NA, frage1 / 12),
         age_first_birth = case_when(is.na(frage) ~ NA,
                                     frage < 20 ~ "< 19",
                                     frage < 25 ~ "20-24",
                                     frage < 30 ~ "25-29",
                                     frage < 35 ~ "30-34",
                                     frage < 40 ~ "35-39",
                                     TRUE ~ "> 40"),
         age_first_birth = fct_relevel(age_first_birth, 
                                       "< 19", "20-24", "25-29", "30-34", "35-39", "> 40"),
         educational_level = case_when(educ %in% 2:32 ~ "Basic Education",
                                       educ %in% 40:73 ~ "Secondary Education",
                                       educ %in% 80:100 ~ "Tertiary Education",
                                       educ %in% 110:125 ~ "University Degree"))

# Saving CPS fertility datasets
path_data_women_1980_2022 <- "refined/cps_women_1980_2022.csv"

write.csv(data_women_1980_2022, file = path_data_women_1980_2022, row.names = FALSE)
