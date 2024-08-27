###########################################################################
# Script: extract_cps_fertility.R
# Author: Gustavo Luchesi
# Last Updated: 8/27/2024
# Description: Extracts CPS dat file from raw and creates intermediate datasets
# based on the Fertility Supplement of the CPS

# Input: cps_00002.dat

# Output: cps_total_fertility.csv and cps_fertility_1980_2022.csv
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

# Generating a dataset for mothers from 1980 - 2022
data_mothers_1980_2022 <- data %>% 
  
  filter(YEAR %in% c(1980, 1990, 1995, 2012, 2014, 2016, 2018, 2020, 2022),
         SEX == 2,
         !(FREVER %in% c(0, 999))) %>% 
  
  mutate(FRAGE = FRAGE1 / 12,
         AGE_FIRST_BIRTH = case_when(FRAGE < 20 ~ "< 19",
                                    FRAGE < 25 ~ "20-24",
                                    FRAGE < 30 ~ "25-29",
                                    FRAGE < 35 ~ "30-34",
                                    FRAGE < 40 ~ "35-39",
                                    FRAGE < 45 ~ "40-44",
                                    TRUE ~ "> 45"),
         AGE_FIRST_BIRTH = fct_relevel(AGE_FIRST_BIRTH, 
                                       "< 19", "20-24", "25-29", "30-34", "35-39", "40-44", "> 45"),
         EDUCATIONAL_LEVEL = case_when(EDUC %in% 2:32 ~ "Basic Education",
                                       EDUC %in% 40:73 ~ "Secondary Education",
                                       EDUC %in% 80:100 ~ "Tertiary Education",
                                       EDUC %in% 110:125 ~ "University Degree"))

# Generating numbers for Fertility by Age at First Birth and Educational Attainment
total_fertility <- data_mothers_1980_2022 %>% 
  
  group_by(YEAR, AGE_FIRST_BIRTH, EDUCATIONAL_LEVEL) %>% 
  summarise(CHILDREN_PER_WOMAN = weighted.mean(FREVER, WTFINL),
            CHILDREN_PER_WOMAN_UNW = mean(FREVER)) %>% 
  ungroup()

# Saving CPS fertility datasets
path_data_mothers_1980_2022 <- "refined/cps_fertility_1980_2022.csv"
path_total_fertility <- "refined/cps_total_fertility.csv"

write.csv(data_mothers_1980_2022, file = path_data_mothers_1980_2022, row.names = FALSE)
write.csv(total_fertility, file = path_total_fertility, row.names = FALSE)