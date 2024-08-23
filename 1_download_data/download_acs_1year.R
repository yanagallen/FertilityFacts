###########################################################################
# Script: download_acs_1year.R
# Author: Gustavo Luchesi
# Last Updated: 8/22/2024
# Description: Download and extract ACS PUMS csv files to raw_data folder

# Input: ACS PUMS 2000 - 2022 1 year zip files

# Output: ACS PUMS 2000 - 2022 1 year csv files 
###########################################################################

# Installing required packages
packages <- c("tidyverse")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())

# Define temporary directory and temporary file for zip files
tempDir <- tempdir()
tempFile <- tempfile()
options(timeout = 500)

# Define prefix, years, and variables to download 
url <- "https://www2.census.gov/programs-surveys/acs/data/pums/"
years <- 2000:2022
vars <- c("SERIALNO", "PWGTP", "PUMA", "SEX", "AGEP", "FER")


for(i in 1:length(years)){
  
  # Create url for each year
  usfile.url <- paste0(url, years[i], ifelse(years[i] >= 2007, "/1-Year/csv_pus.zip", "/csv_pus.zip"))

  # Download US file
  download.file(url = usfile.url, destfile = tempFile)
  
  if (years[i] >= 2006) {
    
    # Name of csv files in zip folder
    usafile <- paste0("ss", substring(years[i], 3, 4), "pusa.csv")
    usbfile <- paste0("ss", substring(years[i], 3, 4), "pusb.csv")
    
    # Unzip and read US files
    usa <- read.csv(unzip(zipfile = tempFile, files = usafile, exdir = tempDir))
    usb <- read.csv(unzip(zipfile = tempFile, files = usbfile, exdir = tempDir))
    
    # Select only variables that will be used
    usa <- usa %>% select(all_of(vars))
    usb <- usb %>% select(all_of(vars))
    
    # Rds file name
    csvfile <- paste0("./raw_data/", "pus", years[i], ".csv")
    
    # Consolidate
    usabpr <- rbind(usa, usb)
    
    # Save files
    write.csv(usabpr, csvfile)
    rm(usabpr, usa, usb)
    
  } else {
    
    # Name of csv files in zip folder
    usfile <- paste0(ifelse(years[i] == 2000, "c2ss", "ss"), 
                     ifelse(years[i] == 2000, "", substring(years[i], 3, 4)), 
                     "pus.csv")
    
    # Unzip and read US files
    us <- read.csv(unzip(zipfile = tempFile, files = usfile, exdir = tempDir))
    
    # Select only variables that will be used
    us <- us %>% select(all_of(vars))
    
    # RDS file name
    csvfile <- paste0("./raw_data/", "pus", years[i], ".csv")
    
    # Save files
    write.csv(us, csvfile)
    rm(us)
    
  }
  
  file.remove(dir(tempDir, pattern = "^ss", full.names = TRUE))
  
}


