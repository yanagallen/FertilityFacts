###########################################################################
# Script: acs_nchildren_education.R
# Author: Gustavo Luchesi
# Last Updated: 10/16/2024
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

# Load intermediate datasets
load("refined/acs_educ_groups.rdata")
load("refined/cps_educ_groups.rdata")

#===========================================================================
# Function for mean number of children in the HH by education group
#===========================================================================

hhchildren.lineplot <- function(data, group_var, start_age, end_age){
  
  grouped_data <- data %>% 
    filter(!is.na(.data[[group_var]]),
           AGE %in% c(start_age:end_age),
           YEAR >= 2000) %>% 
    group_by(YEAR, .data[[group_var]]) %>% 
    summarise(n_child = weighted.mean(NCHILD, w = PERWT)) %>% 
    ungroup()
  
  
  grouped_data %>% 
    ggplot(aes(x = YEAR, y = n_child, color = .data[[group_var]])) +
    geom_point(size = 2.5) +
    geom_line(linewidth = 0.75) +
    scale_color_paletteer_d("ggthemes::Red_Blue_Brown") +
    scale_x_continuous(breaks = seq(min(grouped_data$YEAR), max(grouped_data$YEAR), by = 2)) +
    labs(title = paste0("Number of own children living in HH (Women aged ", start_age, "-", end_age, ")"),
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
  
}

#===========================================================================
# Generating line plots for different age groups
#===========================================================================

# Initial Classification

start_ages <- c(45, 45, 43, 40, 45)
end_ages <- c(45, 55, 47, 44, 50)

for (i in 1:length(start_ages)) {
  
  lineplot <- hhchildren.lineplot(data_educ, "educ_group", start_age =  start_ages[i], end_age =  end_ages[i])
  
  figure_path <- paste0("figures/line_hhchildren_", start_ages[i], "_", end_ages[i], ".jpeg")
  overleaf_path <- paste0("overleaf/line_hhchildren_", start_ages[i], "_", end_ages[i], ".jpeg")
  ggsave(figure_path, plot = lineplot, dpi = 320, width = 8, height = 5)
  ggsave(overleaf_path, plot = lineplot, dpi = 320, width = 8, height = 5)
  
}

# Generating line plots for different age groups (Simplified Measure)

for (i in 1:length(start_ages)) {
  
  lineplot <- hhchildren.lineplot(data_educ, "educ_group2", start_age =  start_ages[i], end_age =  end_ages[i])
  
  figure_path <- paste0("figures/line_hhchildren1_", start_ages[i], "_", end_ages[i], ".jpeg")
  overleaf_path <- paste0("overleaf/line_hhchildren1_", start_ages[i], "_", end_ages[i], ".jpeg")
  ggsave(figure_path, plot = lineplot, dpi = 320, width = 8, height = 5)
  ggsave(overleaf_path, plot = lineplot, dpi = 320, width = 8, height = 5)
  
}

# Generating line plots for different age groups (Doepke Measure)

for (i in 1:length(start_ages)) {
  
  lineplot <- hhchildren.lineplot(data_educ, "educ_simplified", start_age =  start_ages[i], end_age =  end_ages[i])
  
  figure_path <- paste0("figures/line_hhchildren2_", start_ages[i], "_", end_ages[i], ".jpeg")
  overleaf_path <- paste0("overleaf/line_hhchildren2_", start_ages[i], "_", end_ages[i], ".jpeg")
  ggsave(figure_path, plot = lineplot, dpi = 320, width = 8, height = 5)
  ggsave(overleaf_path, plot = lineplot, dpi = 320, width = 8, height = 5)
  
}

#===========================================================================
# Generating bar plots including specific years
#===========================================================================

# Simplified Education Measure
grouped_data <- data_educ %>% 
  mutate(educ_group2 = fct_relevel(educ_group2, c("Less than college", "Some college", "Bachelor's",
                                   "Master's (Lawyer/Doctor)", "Master's", "PhD"))) %>% 
  filter(YEAR %in% c(1990, 2000, 2010, 2020),
         AGE %in% c(45:50)) %>% 
  group_by(YEAR, educ_group2) %>% 
  summarise(hh_nchildren = weighted.mean(NCHILD, w = PERWT)) %>% 
  ungroup()

col_plot <- grouped_data %>% 
  ggplot(aes(x = educ_group2, y = hh_nchildren, fill = as.factor(YEAR))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "Year:",
       title = "Mean number of own children in HH (Women aged 45-50)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank())

figure2_path <- "figures/barplot_hhchildren_45_50.jpeg"
overleaf2_path <- "overleaf/barplot_hhchildren_45_50.jpeg"
ggsave(figure2_path, plot = col_plot, dpi = 320, width = 8, height = 5)
ggsave(overleaf2_path, plot = col_plot, dpi = 320, width = 8, height = 5)

# Doepke Measure

grouped_data2 <- data_educ %>% 
  mutate(educ_simplified = fct_relevel(educ_simplified, c("< 12", "12", "13-15", "16", "> 16"))) %>% 
  filter(YEAR %in% c(1990, 2000, 2010, 2020),
         AGE %in% c(45:50)) %>% 
  group_by(YEAR, educ_simplified) %>% 
  summarise(hh_nchildren = weighted.mean(NCHILD, w = PERWT)) %>% 
  ungroup()

col_plot2 <- grouped_data2 %>% 
  ggplot(aes(x = educ_simplified, y = hh_nchildren, fill = as.factor(YEAR))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "Year:",
       x = "Years of Schooling",
       title = "Mean number of own children in HH (Women aged 45-50)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())

figure3_path <- "figures/barplot_hhchildren1_45_50.jpeg"
overleaf3_path <- "overleaf/barplot_hhchildren1_45_50.jpeg"
ggsave(figure3_path, plot = col_plot2, dpi = 320, width = 8, height = 5)
ggsave(overleaf3_path, plot = col_plot2, dpi = 320, width = 8, height = 5)

#===========================================================================
# Generating plots for children ever born (1970-1990) by maternal education
#===========================================================================

# 45 to 50 years of age
completed_fertility_45_50 <- data_educ %>% 
  mutate(educ_simplified = fct_relevel(educ_simplified, c("< 12", "12", "13-15", "16", "> 16"))) %>% 
  
  filter(YEAR %in% c(1970:1990),
         AGE %in% c(45:50)) %>% 
  group_by(YEAR, educ_simplified) %>% 
  summarise(nchildren_born = weighted.mean(CHBORN, w = PERWT),
            records = n()) %>% 
  ungroup()

completed_fertility_plot_45_50 <- completed_fertility_45_50 %>% 
  
  ggplot(aes(x = educ_simplified, y = nchildren_born,
             group = YEAR, color = factor(YEAR), shape = factor(YEAR))) + 
  geom_line(aes(linetype = factor(YEAR)), linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("1970" = "#1b9e77", 
                                "1980" = "#7570b3", 
                                "1990" = "#e31a1c")) +
  scale_shape_manual(values = c(15, 17, 18, 8, 16)) + 
  scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "twodash", "solid")) +
  labs(x = "Years of Schooling", y = "Completed Fertility", 
       color = "Year", shape = "Year", linetype = "Year",
       title = "Children ever born (Women aged 45-50)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank())
  
figure4_path <- "figures/completed_fetility_45_50.jpeg"
overleaf4_path <- "overleaf/completed_fetility_45_50.jpeg"
ggsave(figure4_path, plot = completed_fertility_plot_45_50, dpi = 320, width = 7, height = 5)
ggsave(overleaf4_path, plot = completed_fertility_plot_45_50, dpi = 320, width = 7, height = 5)

rm(completed_fertility_45_50, completed_fertility_plot_45_50)

# 40 to 50 years of age
completed_fertility_40_50 <- data_educ %>% 
  mutate(educ_simplified = fct_relevel(educ_simplified, c("< 12", "12", "13-15", "16", "> 16"))) %>% 
  
  filter(YEAR %in% c(1970:1990),
         AGE %in% c(40:50)) %>% 
  group_by(YEAR, educ_simplified) %>% 
  summarise(nchildren_born = weighted.mean(CHBORN, w = PERWT),
            records = n()) %>% 
  ungroup()

completed_fertility_plot_40_50 <- completed_fertility_40_50 %>% 
  
  ggplot(aes(x = educ_simplified, y = nchildren_born,
             group = YEAR, color = factor(YEAR), shape = factor(YEAR))) + 
  geom_line(aes(linetype = factor(YEAR)), linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("1970" = "#1b9e77", 
                                "1980" = "#7570b3", 
                                "1990" = "#e31a1c")) +
  scale_shape_manual(values = c(15, 17, 18, 8, 16)) + 
  scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "twodash", "solid")) +
  labs(x = "Years of Schooling", y = "Completed Fertility", 
       color = "Year", shape = "Year", linetype = "Year",
       title = "Children ever born (Women aged 40-50)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank())

figure5_path <- "figures/completed_fetility_40_50.jpeg"
overleaf5_path <- "overleaf/completed_fetility_40_50.jpeg"
ggsave(figure5_path, plot = completed_fertility_plot_40_50, dpi = 320, width = 7, height = 5)
ggsave(overleaf5_path, plot = completed_fertility_plot_40_50, dpi = 320, width = 7, height = 5)

rm(completed_fertility_40_50, completed_fertility_plot_40_50)

#===========================================================================
# Generating plots for children ever born from CPS data
#===========================================================================

completed_fertility_cps <- cpsdata_educ %>% 
  filter(year %in% c(1992, 2000, 2010, 2016),
         age %in% c(40:44)) %>% 
  group_by(year, educ_group3) %>% 
  summarise(nchildren_born = weighted.mean(frever, w = wtfinl),
            records = n()) %>% 
  ungroup()
  
completed_fertility_plot_cps <- completed_fertility_cps %>% 
  
  ggplot(aes(x = educ_group3, y = nchildren_born,
             group = year, color = factor(year), shape = factor(year))) + 
  geom_line(aes(linetype = factor(year)), linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("1992" = "#1b9e77", 
                                "2000" = "#7570b3", 
                                "2010" = "#e31a1c",
                                "2016" = "#d95f02")) +
  scale_shape_manual(values = c(15, 17, 18, 8)) + 
  scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "twodash")) +
  labs(x = "Education", y = "Completed Fertility", 
       color = "Year", shape = "Year", linetype = "Year",
       title = "Children ever born (CPS, Women aged 40-44)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 10),
        axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank())

figure6_path <- "figures/completed_fetility_cps_40_44.jpeg"
overleaf6_path <- "overleaf/completed_fetility_cps_40_44.jpeg"
ggsave(figure6_path, plot = completed_fertility_plot_cps, dpi = 320, width = 7, height = 5)
ggsave(overleaf6_path, plot = completed_fertility_plot_cps, dpi = 320, width = 7, height = 5)


