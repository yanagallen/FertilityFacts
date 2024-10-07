###########################################################################
# Script: acs_nchildren_education.R
# Author: Gustavo Luchesi
# Last Updated: 10/7/2024
# Description: Creates figures for number of children per education group

# Input: acs_nchild_educ_groups.csv

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
acs_data <- read.csv("refined/acs_nchild_educ_groups.csv")

acs_data <- acs_data %>% 
  mutate(educ_group = fct_relevel(educ_group, c("PhD", "Master's and non-STEM major", "Master's and STEM major", 
                                                "Master's and Doctor", "Master's and Lawyer", "STEM Bachelor's",
                                                "Non-STEM Bachelor's", "Associate's degree","Some college", 
                                                "Less than college")))

#===========================================================================
# Creating figure of number of children by education group
#===========================================================================

acs_data %>% 
  ggplot(aes(x = YEAR, y = n_child, color = educ_group)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(limits = c(2009, 2022), 
                     breaks = seq(2009, 2022, 2)) +
  scale_color_paletteer_d("ggthemes::Red_Blue_Brown") +
  labs(x = "Year", y = "Average number of children", color = "Education Group:") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust = 0))

