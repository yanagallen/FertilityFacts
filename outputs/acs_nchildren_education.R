###########################################################################
# Script: acs_nchildren_education.R
# Author: Gustavo Luchesi
# Last Updated: 10/14/2024
# Description: Creates figures for number of children per education group

# Input: acs_educ_groups.rdata

# Output: figures/acs_nchildren_education.jpeg
###########################################################################

# Installing required packages
packages <- c("tidyverse", "paletteer")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

# Load intermediate dataset
load("refined/acs_educ_groups.rdata")

#===========================================================================
# Testing alternative ways to group fertility data by education attainment
#===========================================================================

grouped_data <- data_educ %>% 
  
  filter(!is.na(educ_group),
         AGE %in% c(43:47)) %>% 
  
  group_by(YEAR, educ_group) %>% 
  summarise(n_child = weighted.mean(NCHILD, w = PERWT),
            n_sample = n(),
            total_weight = sum(PERWT)) %>% 
  ungroup()

#===========================================================================
# Creating figure of number of children by education group
#===========================================================================

grouped_data %>% 
  ggplot(aes(x = YEAR, y = n_child, color = educ_group)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(limits = c(2009, 2022), 
                     breaks = seq(2009, 2022, 2)) +
  scale_color_paletteer_d("ggthemes::Red_Blue_Brown") +
  labs(title = "Number of own children living in HH",
       x = "Year", color = "Education Group:") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust = 0))

