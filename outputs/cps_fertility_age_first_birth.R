###########################################################################
# Script: cps_fertility_age_first_birth.R
# Author: Gustavo Luchesi
# Last Updated: 8/27/2024
# Description: Extracts CPS dat file from raw and creates intermediate datasets
# based on the Fertility Supplement of the CPS

# Input: cps_total_fertility.csv and cps_fertility_1980_2022.csv

# Output: 
###########################################################################

# Installing required packages
packages <- c("tidyverse", "viridis")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Load intermediate datasets
total_fertility <- read.csv("refined/cps_total_fertility.csv")
mothers_1980_2022 <- read.csv("refined/cps_fertility_1980_2022.csv")

# Generating figures for Total Fertility by Age of First Birth and Educational Level

mothers_1980_2022 <- mothers_1980_2022 %>% 
  mutate(AGE_FIRST_BIRTH = fct_relevel(AGE_FIRST_BIRTH, 
                                       "< 19", "20-24", "25-29", "30-34", "35-39", "40-44", "> 45"))
total_fertility <- total_fertility %>% 
  mutate(AGE_FIRST_BIRTH = fct_relevel(AGE_FIRST_BIRTH, 
                                       "< 19", "20-24", "25-29", "30-34", "35-39", "40-44", "> 45"))
  
## Total Fertility by Age of First Birth
mothers_1980_2022 %>% 
  
  filter(YEAR %in% (2012:2022)) %>% 
  
  group_by(YEAR, AGE_FIRST_BIRTH) %>%
  summarise(CHILDREN_PER_WOMAN = weighted.mean(FREVER, WTFINL)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = YEAR, y = CHILDREN_PER_WOMAN, color = AGE_FIRST_BIRTH)) +
  geom_line() +
  geom_point() +
  theme_minimal()

## Total Fertility by Age of First Birth and Educational Level
total_fertility %>% 
  filter(YEAR %in% c(1980, 2022)) %>% 
  
  ggplot(aes(x = AGE_FIRST_BIRTH, y = CHILDREN_PER_WOMAN, fill = EDUCATIONAL_LEVEL)) +
  geom_col(position = "dodge") +
  facet_wrap(~ YEAR) +
  labs(x = "Age",
       y = "Children per woman", 
       fill = "Education:",
       caption = "Source: Fertility Supplement of the Current Population Survey") +
  theme_minimal() +
  theme(legend.position = "bottom")

