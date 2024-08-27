###########################################################################
# Script: wb_tfr_across_countries.R
# Author: Gustavo Luchesi
# Last Updated: 8/27/2024
# Description: Download and extract World Bank fertility data to raw_data folder

# Input: wb_tfr_1960_2022.csv

# Output: Total Fertility Rate figures
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

# Loading WB data
tfr_data <- read.csv("refined/wb_tfr_1960_2022.csv")

# Total Fertility Rate (TFR) across selected countries and over time

tfr_line_plot <- function(tfr_data, countries, start_year, end_year) {
  
  tfr_data %>% 
    filter(country %in% countries,
           year %in% start_year:end_year) %>% 
    
    ggplot(aes(x = year, y = tfr, color = country)) +
    geom_line(linewidth = 1.25) +
    scale_y_continuous(limits = c(0.5, NA)) +
    labs(title = paste0("Total Fertility Rate (", start, "-", end, ")")) +
    scale_color_paletteer_d("ggthemes::Red_Blue_Brown") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.box.spacing = ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.line = element_line(),
          axis.ticks = element_line(),
          panel.grid = element_blank()) +
    guides(col = guide_legend(nrow = 2))
  
}

# Figure Parameters
start <- 1960
end <- 2022
data <- tfr_data

main <- c("United States", "United Kingdom", "Belgium", "Italy", "Japan", "Austria",
          "Denmark", "Switzerland", "Spain", "Sweden", "Korea, Rep.", "Germany")

latin_america <-  c("Brazil", "Argentina", "Chile", "Venezuela, RB", 
                    "Colombia", "Uruguay", "Peru", "Paraguay")

africa <- c("Nigeria", "South Africa", "Algeria", "Egypt, Arab Rep.", 
            "Ethiopia", "Congo, Dem. Rep.", "Kenya", "Sudan")

asia <- c("China", "Japan", "Viet Nam", "Indonesia",
          "Korea, Rep.", "India", "Pakistan", "Bangladesh")

europe <- c("Germany", "France", "Spain", "Italy",
            "United Kingdom", "Switzerland", "Austria", "Sweden")

brics <- c("China", "Brazil", "India", "South Africa", 
           "Russian Federation", "Iran, Islamic Rep.", "Egypt, Arab Rep.", "United Arab Emirates")

plot_list <- list(main, latin_america, africa, asia, europe, brics)
plot_names <- c("main", "latin_america", "africa", "asia", "europe", "brics")

# Generating figures

for(i in 1:length(plot_list)) {
  
  figure <- tfr_line_plot(data, plot_list[[i]], start, end)
  
  figure_path <- paste0("figures/wb_tfr_", plot_names[i], ".jpeg")
  
  ggsave(filename = figure_path, plot = figure, dpi = 320, width = 6, height = 5)
  
}

