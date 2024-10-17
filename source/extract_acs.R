###########################################################################
# Script: extract_acs.R
# Author: Gustavo Luchesi
# Last Updated: 10/16/2024
# Description: Extracts ACS PUMS dat file from raw and creates intermediate datasets
# based on the American Community Survey PUMS

# Input: 
# usa_00001.dat (ACS PUMS 2000 - 2022 1 year dat file from raw)
# usa_00004.dat

# Output: 
# acs_percent_pregnant_cohorts.csv
# acs_educ_groups.rdata
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
ddi_educ <- read_ipums_ddi("raw/usa_00004.xml")
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
                                EDUCD %in% c(114:115) & OCC2010 == 2100 ~ "Master's (Lawyer)",
                                EDUCD %in% c(114:115) & OCC2010 == 3060 ~ "Master's (Doctor)",
                                EDUCD %in% c(114:115) & !OCC2010 %in% c(2100, 3060) & stem_degree == 1 ~ "Master's and STEM",
                                EDUCD %in% c(114:115) & !OCC2010 %in% c(2100, 3060) & stem_degree == 0 ~ "Master's and non-STEM",
                                EDUCD == 116 ~ "PhD"),
         educ_group = fct_relevel(educ_group, c("PhD", "Master's and non-STEM", "Master's and STEM", 
                                                "Master's (Doctor)", "Master's (Lawyer)", "STEM Bachelor's",
                                                "Non-STEM Bachelor's", "Associate's degree", "Some college", 
                                                "Less than college")),
         
         educ_group1 = case_when(EDUCD <= 64 ~ "Less than college",
                                 EDUCD %in% c(65:100) ~ "Some college",
                                 EDUCD == 101 ~ "Bachelor's",
                                 EDUCD %in% c(114:115) & OCC2010 == 2100 ~ "Master's (Lawyer)",
                                 EDUCD %in% c(114:115) & OCC2010 == 3060 ~ "Master's (Doctor)",
                                 EDUCD %in% c(114:115) & !OCC2010 %in% c(2100, 3060) ~ "Master's",
                                 EDUCD == 116 ~ "PhD"),
         educ_group1 = fct_relevel(educ_group1, c("PhD", "Master's",
                                                  "Master's (Doctor)", "Master's (Lawyer)", "Bachelor's",
                                                  "Some college", "Less than college")),
         
         educ_group2 = case_when(EDUCD <= 64 ~ "Less than college",
                                 EDUCD %in% c(65:100) ~ "Some college",
                                 EDUCD == 101 ~ "Bachelor's",
                                 EDUCD %in% c(114:115) & OCC2010 %in% c(2100, 3060) ~ "Master's (Lawyer/Doctor)",
                                 EDUCD %in% c(114:115) & !OCC2010 %in% c(2100, 3060) ~ "Master's",
                                 EDUCD == 116 ~ "PhD"),
         educ_group2 = fct_relevel(educ_group2, c("PhD", "Master's",
                                                  "Master's (Lawyer/Doctor)", "Bachelor's",
                                                  "Some college", "Less than college")),
         
         educ_simplified = case_when(EDUC < 6 ~ "< 12",
                                     EDUC == 6 ~ "12",
                                     EDUC %in% c(7:9) ~ "13-15",
                                     EDUC == 10 ~ "16",
                                     EDUC == 11 ~ "> 16"),
         educ_simplified = fct_relevel(educ_simplified, c("> 16", "16", "13-15", "12", "< 12"))) 

# Saving intermediate dataset
path_educ = "refined/acs_educ_groups.rdata"
save(data_educ, file = path_educ)
