###########################################################################
# Script: acs_births_across_cohorts.R
# Author: Gustavo Luchesi
# Last Updated: 8/26/2024
# Description: Creates figures for births by mother's age across cohorts

# Input: acs_percent_pregnant_cohorts.csv

# Output: Figures based on ACS data
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

# Load intermediate dataset
acs_data <- read.csv("refined/acs_percent_pregnant_cohorts.csv")

# Fertility by age across cohorts
acs_figure <- acs_data %>% 
  ggplot(aes(x = AGE, y = PERCENT_PREGNANT, color = YEAR, group = YEAR)) +
  geom_line() +
  labs(title = "Did you give birth to a child in the last 12 months?",
       x = "Age of Woman",
       color = "Year") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     limits = c(0, 0.13),
                     n.breaks = 6) +
  scale_color_viridis(option = "D",
                      begin = 1,
                      end = 0) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.line = element_line())

# Saving figures in output folder
figure_path <- "figures/acs_births_across_years.jpeg"
ggsave(figure_path, plot = acs_figure)




