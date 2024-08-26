###########################################################################
# Script: download_wb_data.R
# Author: Gustavo Luchesi
# Last Updated: 8/22/2024
# Description: Download and extract World Bank fertility data to raw_data folder

# Input: World Bank raw files

# Output: World Bank csv files
###########################################################################

# Installing required packages
packages <- c("tidyverse")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

































