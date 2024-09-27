###########################################################################
# Script: oecd_spending_tfr.R
# Author: Gustavo Luchesi
# Last Updated: 9/27/2024
# Description: Creates figures for Policy Spending and Fertility Rates across 
# OECD countries

# Inputs: oecd_spending_data.csv  wb_trf_1960_2022.csv

# Outputs: 
# figures/scatter_tfr_pct_change_spending.jpeg
# figures/scatter_tfr_pct_change_spending_to_gdp.jpeg
# figures/scatter_tfr_pc_family_spending_2015usd.jpeg
# figures/scatter_tfr_family_spending_to_gdp.jpeg

###########################################################################

# Installing required packages
packages <- c("tidyverse", "paletteer", "ggrepel", "scales")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/gustavoml/Desktop/Projects/FertilityFacts")

#===============================================================================
# Loading WB and OECD data
#===============================================================================

spending_data <- read.csv("refined/oecd_spending_data.csv")
tfr_data <- read.csv("refined/wb_tfr_1960_2022.csv")

# Storing the countries for which we have spending and fertility data
oecd_countries <- unique(spending_data$country_code)

# Merging the datasets
oecd_data <- tfr_data %>% 
  
  left_join(spending_data, by = c("country_code", "year")) %>% 
  
  filter(country_code %in% oecd_countries)

#===============================================================================
# Creating the policy spending and TFR sample for figures
#===============================================================================

period <- c(1990, 2019)

plot_data <- oecd_data %>% 
  filter(year %in% period) %>% 
  
  group_by(country_code) %>% 
  mutate(pct_change_spending = pc_family_spending_2015usd / lag(pc_family_spending_2015usd) - 1,
         pct_change_spending_to_gdp = family_spending_to_gdp / lag(family_spending_to_gdp) - 1,
         pct_change_tfr = (tfr / lag(tfr)) - 1) %>%
  ungroup() %>%
  
  filter(year == 2019) 

#===============================================================================
# Generating scatter plots - Levels
#===============================================================================

basic_scatter <- function(data, countries, x_var, label) {
  
  data %>% 
    filter(country_code %in% countries) %>% 
    
    ggplot(aes(x = .data[[x_var]], y = tfr, label = country_code)) +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(limits = c(1, 2)) +
    labs(x = label,
         y = "Total Fertility Rate") +
    theme_minimal() +
    theme(axis.text = element_text(size = 10),
          axis.line = element_line(),
          axis.ticks = element_line(),
          panel.grid = element_blank())
  }

peers <- c("ESP", "FRA", "DEU", "FIN", "NOR", "SWE", "GBR", "GRC", "JPN", "AUS", "CAN",
           "DNK", "ITA", "NLD", "USA", "CHE", "BEL", "AUT", "IRL", "ISL", "CZE","PRT", 
           "POL")

independent_vars <- c("family_spending_to_gdp", 
                      "pc_family_spending_2015usd")
independent_vars_labels <- c("Public Spending on Family Policies (% of GDP)", 
                             "Per Capita Public Spending on Family Policies in 2019 (2015 USD PPP)")

#===============================================================================
# Saving figures in folder
#===============================================================================

for(i in 1:length(independent_vars)) {
  
  figure <- basic_scatter(plot_data, peers, independent_vars[i], independent_vars_labels[i])
  
  path <- paste0("figures/scatter_tfr_", independent_vars[i], ".jpeg")
  path_overleaf <- paste0("overleaf/scatter_tfr_", independent_vars[i], ".jpeg")
  
  ggsave(filename = path, plot = figure, dpi = 320, width = 6, height = 5)
  ggsave(filename = path_overleaf, plot = figure, dpi = 320, width = 6, height = 5)
  
}

#===============================================================================
# Generating scatter plots - Changes between 1990 and 2019
#===============================================================================

change_scatter <- function(data, countries, x_var, label) {
  
  data %>% 
    filter(country_code %in% countries) %>% 
    
    ggplot(aes(x = .data[[x_var]], y = pct_change_tfr, label = country_code)) +
    geom_point() +
    geom_text_repel() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.1) +
    scale_x_continuous(limits = c(-0.5, NA),
                       labels = scales::percent) +
    scale_y_continuous(limits = c(-0.35, 0.1),
                       labels = scales::percent) +
    labs(x = label,
         y = "Change in TFR Between 1990 and 2019") +
    theme_minimal() +
    theme(axis.text = element_text(size = 10),
          axis.line = element_line(),
          axis.ticks = element_line(),
          panel.grid = element_blank())
}


independent_vars_c <- c("pct_change_spending_to_gdp", 
                        "pct_change_spending")
independent_vars_labels_c <- c("Change in Public Spending as % of GDP", 
                               "Change in Per Capita Public Spending Between 1990 and 2019")

#===============================================================================
# Saving figures in folder
#===============================================================================

for(i in 1:length(independent_vars_c)) {
  
  figure <- change_scatter(plot_data, peers, independent_vars_c[i], independent_vars_labels_c[i])
  
  path <- paste0("figures/scatter_tfr_", independent_vars_c[i], ".jpeg")
  path_overleaf <- paste0("overleaf/scatter_tfr_", independent_vars_c[i], ".jpeg")
  
  ggsave(filename = path, plot = figure, dpi = 320, width = 6, height = 5)
  ggsave(filename = path_overleaf, plot = figure, dpi = 320, width = 6, height = 5)
  
}
