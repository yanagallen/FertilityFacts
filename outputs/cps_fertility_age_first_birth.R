###########################################################################
# Script: cps_fertility_age_first_birth.R
# Author: Gustavo Luchesi
# Last Updated: 8/30/2024
# Description: Extracts CPS dat file from raw and creates intermediate datasets
# based on the Fertility Supplement of the CPS

# Input: cps_total_fertility.csv and cps_fertility_1980_2022.csv

# Output: figures/cps_total_fertility.jpeg
###########################################################################

# Installing required packages
packages <- c("tidyverse", "viridis", "knitr", "kableExtra")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Load intermediate datasets
women_1980_2022 <- read.csv("refined/cps_women_1980_2022.csv")

women_1980_2022 <- women_1980_2022 %>% 
  mutate(age_first_birth = fct_relevel(age_first_birth, 
                                       "< 19", "20-24", "25-29", "30-34", "35-39", "> 40"))

# Generating numbers for Fertility by Age at First Birth and Educational Attainment
total_fertility <- women_1980_2022 %>% 
  
  # Filter only women who ever gave birth and who have completed their fertility (between 45 and 50 y.o. for comparability)
  filter(!(frever %in% c(0, 999)),  
         age %in% c(45:50)) %>% 
  
  group_by(year, age_first_birth) %>% 
  summarise(children_per_woman = weighted.mean(frever, wtfinl),
            children_per_woman_unw = mean(frever),
            n_women = n()) %>% 
  
  ungroup()

# Generating figure for Total Fertility by Age of First Birth
  
## Total Fertility by Age of First Birth
plot_fertility <- total_fertility %>% 
  
  filter(year %in% c(1980, 1990, 2016)) %>% 
  
  ggplot(aes(x = age_first_birth, y = children_per_woman, fill = as.factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Age at First Birth",
       y = "Children per woman", 
       fill = "Year:") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank())

## Table - Fertility by Age of First Birth

wide_fertility <- total_fertility %>% 
  filter(year %in% c(1980, 1990, 2016)) %>% 
  select(-children_per_woman_unw) %>% 
  pivot_wider(names_from = year, values_from = c(children_per_woman, n_women)) 

sample_size <- wide_fertility %>% 
  select(age_first_birth, starts_with("n_")) %>% 
  mutate(across(starts_with("n_"), ~ as.double(.x))) %>% 
  rename_with(~ sub("n_women_", "", .), starts_with("n_"))

total_fertility_by_age <- wide_fertility %>% 
  select(age_first_birth:children_per_woman_2016) %>% 
  rename_with(~ sub("children_per_woman_", "", .), starts_with("children_")) 

table <- rbind(total_fertility_by_age, sample_size) %>% 
  arrange(age_first_birth)


# Saving the generated figures
path <- "figures/cps_total_fertility.jpeg"
path_overleaf <- "overleaf/cps_total_fertility.jpeg"

ggsave(filename = path, plot = plot_fertility, dpi = 320, width = 6, height = 5)
ggsave(filename = path_overleaf, plot = plot_fertility, dpi = 320, width = 6, height = 5)


