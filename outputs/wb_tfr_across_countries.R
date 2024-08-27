###########################################################################
# Script: wb_tfr_across_countries.R
# Author: Gustavo Luchesi
# Last Updated: 8/27/2024
# Description: Download and extract World Bank fertility data to raw_data folder

# Input: wb_tfr_1960_2022.csv

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

# Loading WB data
tfr_data <- read.csv("refined/wb_tfr_1960_2022.csv")




