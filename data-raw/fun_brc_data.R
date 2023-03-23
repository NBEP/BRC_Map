################################### HEADER ###################################
#  TITLE: fun_brc_data.R
#  DESCRIPTION: Formats numerical data for BRC_Map, calculates score
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-23
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(lubridate)
library(tidyverse)

brcmap_format_data <- function(parameters_db, sites_db, data_num_db,
                               data_text_db, keep_ecoli=FALSE, max_year,
                               output_path) {

  # Process numeric data ----
  # * Add columns ----
  df_data_num <- readRDS(data_num_db) %>%
    # Set DATE_TIME to datetime format
    mutate(DATE_TIME = ymd_hm(DATE_TIME, tz = "America/New_York")) %>%
    # Drop non-QAQC data (all years before max_year)
    filter(lubridate::year(DATE_TIME) <= max_year) %>%
    # Drop extra columns
    select(SITE_BRC_CODE, DATE_TIME, PARAMETER, RESULT, UNITS) %>%
    # Rename columns
    rename(BRC_CODE = SITE_BRC_CODE) %>%
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

  # * Drop E. coli data ----
  if(keep_ecoli==FALSE){
    df_data_num <- df_data_num %>%
      filter(!grepl('E. coli', PARAMETER))
  }

  # * Calc avg for replicates ----
  # Make list of parameters with replicate values
  df_replicates <- df_data_num %>%
    filter(SAMPLE_TYPE == 'Replicate')

  list_replicates <- unique(df_replicates$PARAMETER)

  # Calculate average
  df_composite <- df_data_num %>%
    # Only select data with replicates
    filter(PARAMETER %in% list_replicates,
           SAMPLE_TYPE %in% c('Grab', 'Replicate'),
           RESULT != -999999) %>%
    # Group data, calc average
    group_by(BRC_CODE, DATE_TIME, PARAMETER, UNITS) %>%
    summarise(RESULT = mean(RESULT),
              .groups = 'drop') %>%
    # Set SAMPLE_TYPE to 'Composite'
    mutate(SAMPLE_TYPE = 'Composite')

  # * Add composite scores to numeric data ----
  brc_data_num <- bind_rows(df_data_num, df_composite)

  # * Save output ----
  save(brc_data_num, file=file.path(output_path, 'brc_data_num.rda'))

  # Process qualitative data ----
  brc_data_text <- readRDS(data_text_db) %>%
    # Set DATE_TIME to datetime format
    mutate(DATE_TIME = ymd_hm(DATE_TIME, tz = "America/New_York")) %>%
    # Drop non-QAQC data (all years before max_year)
    filter(lubridate::year(DATE_TIME) <= max_year) %>%
    # Drop extra columns
    select(SITE_BRC_CODE, DATE_TIME, PARAMETER, RESULT) %>%
    # Rename columns
    rename(BRC_CODE = SITE_BRC_CODE) %>%
    # Drop rows
    filter(!PARAMETER %in% c('Sampler', 'Entered By'))

  # * Save output ----
  save(brc_data_text, file=file.path(output_path, 'brc_data_text.rda'))

  # Calculate score ----
  # * Process numeric data ----
  df_data_num <- brc_data_num %>%
    # Drop extra rows
    filter(SAMPLE_TYPE %in% c('Grab', 'Composite'),
           RESULT != -999999,
           !PARAMETER %in% c('Air Temperature', 'Water Depth')) %>%
    # Sort by SAMPLE_TYPE
    arrange(DATE_TIME, BRC_CODE, PARAMETER, SAMPLE_TYPE) %>%
    # Group data, take first entry (composite if available, otherwise grab)
    group_by(BRC_CODE, DATE_TIME, PARAMETER, UNITS) %>%
    summarise(RESULT = first(RESULT),
              SAMPLE_TYPE = first(SAMPLE_TYPE),
              .groups = 'drop') %>%
    # Add year
    mutate(YEAR = lubridate::year(DATE_TIME))

  # * Process site data ----
  df_site <- readRDS(sites_db) %>%
    select(BRC_CODE, CFR, CONDUCTIVITY_USCM) %>%
    # Fail safe: replace missing CFR data with 'No'
    mutate(CFR = replace_na(CFR, 'No'))

  # * Merge site, numeric data
  df_cwf <- merge(x=df_data_num, y=df_site, by='BRC_CODE')

  # * Additional data formatting
  df_cwf <- df_cwf %>%
    # Calculate relative conductivity
    mutate(RESULT = case_when(PARAMETER=='Conductivity' ~ RESULT - CONDUCTIVITY_USCM,
                                 TRUE ~ RESULT)) %>%
    filter(!is.na(RESULT))

  # * Read in parameter data ----
  parameters_db <- readRDS(parameters_db) %>%
    filter(!is.na(EXC) & !is.na(EXC_CWF) & !is.na(GOOD) & !is.na(GOOD_CWF))

  # * Function: calculate score ----
  brcmap_calculate_score <- function(cwf, parameter, min_score,
                                     max_score, avg_score) {

    # Filter parameter data for selected parameter
    param_db <- parameters_db %>%
      filter(PARAMETER_NAME == parameter)

    # Check for missing parameter and/or data, return score of 'No Data'
    if(nrow(param_db)==0) {

      score = 'No Data'

    } else {

      # Check if cold water fishery
      if (cwf == 'Yes'){
        score_exc <- param_db$EXC_CWF[1]
        score_good <- param_db$GOOD_CWF[1]
        score_fair <- param_db$FAIR_CWF[1]
      } else {
        score_exc <- param_db$EXC[1]
        score_good <- param_db$GOOD[1]
        score_fair <- param_db$FAIR[1]
      }

      # Check whether to use minimum, maximum, or average score
      if (param_db$SCORE[1] == 'min'){
        score_value = min_score
      } else if (param_db$SCORE[1] == 'max') {
        score_value = max_score
      } else {
        score_value = avg_score
      }

      # Check if data ascending or descending...
      if (score_exc > score_good) {
        score = case_when(
          score_value >= score_exc ~ 'Excellent',
          score_value >= score_good ~ 'Good',
          score_value >= score_fair ~ 'Fair',
          score_value < score_fair ~ 'Poor'
        )
      } else if (score_exc < score_good) {
        score = case_when(
          score_value <= score_exc ~ 'Excellent',
          score_value <= score_good ~ 'Good',
          score_value <= score_fair ~ 'Fair',
          score_value > score_fair ~ 'Poor'
        )
      } else {
        score = 'No Data'
      }

    }

    return (score)

  }

  # Vectorize function
  brcmap_calculate_score <- Vectorize(brcmap_calculate_score)

  # * Run calculation ----
  brc_data_score <- df_cwf %>%
    # Group data, calc min/max/avg
    group_by(BRC_CODE, CFR, YEAR, PARAMETER) %>%
    summarize(MIN=min(RESULT),
              MAX=max(RESULT),
              AVG=mean(RESULT),
              .groups = 'drop') %>%
    # Calculate score
    mutate(SCORE=brcmap_calculate_score(CFR, PARAMETER, MIN, MAX, AVG)) %>%
    # Drop extra columns
    select(BRC_CODE, YEAR, PARAMETER, SCORE)

  save(brc_data_score, file=file.path(output_path, 'brc_data_score.rda'))
}

brcmap_format_data(parameters_db='data-raw/parameters_db.rds',
                   sites_db='data-raw/sites_db.rds',
                   data_num_db='data-raw/data_num_db.rds',
                   data_text_db='data-raw/data_text_db.rds',
                   keep_ecoli=FALSE,
                   max_year=2019,
                   output_path='data')
