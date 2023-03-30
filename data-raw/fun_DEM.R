################################### HEADER ###################################
#  TITLE: fun_DEM.R
#  DESCRIPTION: Formats numerical data for DEM
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-30
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(lubridate)
library(tidyverse)
library(writexl)

################################# FUNCTION ###################################

brc_format_DEM <- function(sites_db, data_num_db, keep_ecoli=FALSE, min_year, 
                           max_year, output_path) {
  
  RI_towns = c('Burrillville', 'Central Falls', 'Cumberland', 'Glocester',
               'Lincoln', 'North Smithfield', 'Pawtucket', 'Scituate',
               'Woonsocket')
  
  # Format, filter site data ----
  df_sites <- readRDS(sites_db) %>%
    # Select only RI sites
    filter(TOWN %in% RI_towns) %>%
    # Add STATE column
    mutate(STATE = 'RI') %>%
    # Select columns
    select(SITE_NUMBER, BRC_CODE, SITE_NAME, LATITUDE, LONGITUDE,
           WATERBODY_NAME, TOWN, STATE, HUC12_NUM, HUC_NAME) %>%
    # Tweak column names
    rename(HUC12_NAME=HUC_NAME) %>%
    # Tweak HUC12_NUM, ensure starts with zero
    mutate(HUC12_NUM=ifelse(nchar(HUC12_NUM)<12, paste0("0", HUC12_NUM),
                            HUC12_NUM)) %>%
    # Replace NA with -999999
    replace(is.na(.), -999999) %>%
    mutate(SITE_NUMBER = case_when(SITE_NUMBER=='NA' ~ '-999999',
                                   TRUE ~ SITE_NUMBER))
  
  # Process numeric data ----
  # * Read in data ----
  df_data_num <- readRDS(data_num_db) %>%
      rename(BRC_CODE = SITE_BRC_CODE)
    
  # * Drop E. coli data ----
  if(keep_ecoli==FALSE){
    df_data_num <- df_data_num %>%
      filter(!grepl('E. coli', PARAMETER))
  }
    
  # * Drop all non-RI data ----
  df_merge <- merge(x=df_data_num, y=df_sites, by='BRC_CODE')
  
  # * Add columns ----
  df_data_num <- df_merge %>%
    # Set DATE_TIME to datetime format
    mutate(DATE_TIME = ymd_hm(DATE_TIME, tz = "America/New_York")) %>%
    # Filter for date range, only RI data
    filter(lubridate::year(DATE_TIME) >= min_year,
           lubridate::year(DATE_TIME) <= max_year) %>%
    # Drop extra columns
    select(BRC_CODE, DATE_TIME, PARAMETER, RESULT, UNITS) %>%
    # Add column for "Sample Type"
    mutate(SAMPLE_TYPE = case_when(grepl('Replicate', PARAMETER) ~ 'Replicate',
                                   grepl('Field Blank', PARAMETER) ~ 'Field Blank',
                                   grepl('Lab Blank', PARAMETER) ~ 'Lab Blank',
                                   grepl('Blank', PARAMETER) ~ 'Blank',
                                   TRUE ~ 'Grab')) %>%
    # Drop 'replicate', 'field blank', 'lab blank' from PARAMETER strings
    # Case sensitive!
    mutate(PARAMETER = str_replace_all(PARAMETER,
                                       c("Replicate" = "",
                                         "Field Blank" = "",
                                         "Lab Blank" = "",
                                         "Blank" = ""))) %>%
    # Drop trailing blank spaces
    mutate(PARAMETER = str_trim(PARAMETER))
  
  # * Format columns ----
  df_data_DEM <- df_data_num %>%
    # Rename columns
    rename('Station Name' = BRC_CODE, 'Sample Type'= SAMPLE_TYPE, 
           Parameter = PARAMETER, Concentration = RESULT, Unit = UNITS) %>%
    # Add date, time columns
    mutate(Date = format(DATE_TIME, format="%m/%d/%Y")) %>%
    mutate_at("Date", as.character) %>%
    mutate(Time = format(DATE_TIME, format="%H:%M %Z")) %>%
    mutate_at("Time", as.character) %>%
    # Add columns
    mutate('Sample Media' = 'Water',
           'Depth' = '',
           'Qualifier Code' = '',
           'Detection Limit' = '',
           'Detection Limit Unit' = Unit,
           'Quantitation Level' = '',
           'Quantitation Level Unit' = Unit,
           'Lab Name' = '',
           'Analytical Method Number' = '',
           'Sediment Particle Size' = '',
           'Particle Size Unit' = '',
           'Fish Sample Type' = '',
           'Fish Taxa' = '',
           'Comments' = '') %>%
    # Arrange columns, drop extra columns
    select('Station Name', Date, Time, 'Sample Type', 'Sample Media', Depth,
           Parameter, Concentration, Unit, 'Qualifier Code', 'Detection Limit',
           'Detection Limit Unit', 'Quantitation Level', 
           'Quantitation Level Unit', 'Lab Name', 'Analytical Method Number',
           'Sediment Particle Size', 'Particle Size Unit', 'Fish Sample Type',
           'Fish Taxa', Comments) %>%
    # Sort
    arrange(Date, Time, 'Station Name', Parameter)
  
  # Save output ----
  data_list <- list(CHEM_Template = df_data_DEM,
                    BRC_Sites = df_sites)
  
  for (i in 1:length(data_list)) {
    fileName <- paste0(output_path, names(data_list)[i], ".csv")
    write.csv(data_list[[i]], fileName, row.names=FALSE)
  }
  
}

brc_format_DEM(sites_db='data-raw/sites_db.rds',
                   data_num_db='data-raw/data_num_db.rds',
                   keep_ecoli=FALSE,
                   min_year=2017,
                   max_year=2022,
                   output_path='')
