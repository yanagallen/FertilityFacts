###########################################################################
# Script: extract_acs.R
# Author: Gustavo Luchesi
# Last Updated: 10/7/2024
# Description: Extracts ACS PUMS dat file from raw and creates intermediate datasets
# based on the American Community Survey PUMS

# Input: 
# usa_00001.dat (ACS PUMS 2000 - 2022 1 year dat file from raw)
# usa_00003.dat

# Output: 
# acs_percent_pregnant_cohorts.csv
# acs_nchild_educ_groups.csv
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

#============================================================================================
# Loading Fertility by education ACS sample
#============================================================================================

# Loading extract into R and converting to data.table format
ddi_educ <- read_ipums_ddi("raw/usa_00003.xml")
data_educ <- read_ipums_micro(ddi_educ)

# STEM programs CIP codes
stem_codes <- c("14", "26", "27", "40")

# Fields of degree that changed codes in the CPS between 2009 and 2010
stem_detailed_codes <- c("4003", "4008", "5098")

# Adding a STEM degree dummy
data_educ <- data_educ %>% 
  mutate(stem_degree = case_when(is.na(DEGFIELD) ~ NA,
                                 DEGFIELD %in% stem_codes | DEGFIELDD %in% stem_detailed_codes ~ 1,
                                 TRUE ~ 0))

# Creating the education-occupation groups to generate figures
data_educ <- data_educ %>% 
  mutate(educ_group = case_when(is.na(DEGFIELD) ~ NA,
                                EDUCD <= 64 ~ "Less than college",
                                EDUCD %in% c(65:71) ~ "Some college",
                                EDUCD %in% c(81:100) ~ "Associate's degree",
                                EDUCD == 101 & stem_degree == 0 ~ "Non-STEM Bachelor's",
                                EDUCD == 101 & stem_degree == 1 ~ "STEM Bachelor's",
                                EDUCD %in% c(114:115) & OCC2010 == 2100 ~ "Master's and Lawyer",
                                EDUCD %in% c(114:115) & OCC2010 == 3060 ~ "Master's and Doctor",
                                EDUCD %in% c(114:115) & !OCC2010 %in% c(2100, 3060) & stem_degree == 1 ~ "Master's and STEM major",
                                EDUCD %in% c(114:115) & !OCC2010 %in% c(2100, 3060) & stem_degree == 0 ~ "Master's and non-STEM major",
                                EDUCD == 116 ~ "PhD"),
         educ_group = fct_relevel(educ_group, c("PhD", "Master's and non-STEM major", "Master's and STEM major", 
                                                "Master's and Doctor", "Master's and Lawyer", "STEM Bachelor's",
                                                "Non-STEM Bachelor's", "Associate's degree","Some college", 
                                                "Less than college"))) 

# Collapsing at the education group level
nchild_education <- data_educ %>% 
  filter(YEAR >= 2009) %>% 
  
  group_by(YEAR, educ_group) %>% 
  summarise(n_child = weighted.mean(NCHILD, w = PERWT),
            sample = n()) %>% 
  ungroup()

# Saving intermediate dataset
path_educ = "refined/acs_nchild_educ_groups.csv"
write.csv(nchild_education, file = path_educ, row.names = FALSE)

