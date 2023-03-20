################################### HEADER ###################################
#  TITLE: fun_brc_data.R
#  DESCRIPTION: Formats numerical data for BRC_Map, calculates score
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-20
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(lubridate)
library(tidyverse)

brcmap_format_data <- function(parameters_db, sites_db, data_num_db,
                               output_path) {

  # Format, save numeric data ----

  brc_data <- readRDS(data_num_db) %>%
    # Drop extra columns
    select(SITE_BRC_CODE, DATE_TIME, PARAMETER, RESULT, UNITS) %>%
    # Set DATE_TIME to datetime format
    mutate(DATE_TIME = ymd_hm(DATE_TIME, tz = "America/New_York")) %>%
    # Calc year
    mutate(YEAR = lubridate::year(DATE_TIME)) %>%
    # Filter out null values
    filter(RESULT != -999999) %>%
    # Rename columns
    rename(BRC_CODE = SITE_BRC_CODE)

  save(brc_data, file=file.path(output_path, 'brc_data.rda'))

  # Add CFR column from site data ----
  df_site <- readRDS(sites_db) %>%
    select(BRC_CODE, CFR)

  df_cwf <- merge(x=brc_data, y=df_site, by='BRC_CODE')

  # Read in parameter data ----
  parameters_db <- readRDS(parameters_db) %>%
    filter(!is.na(EXC) & !is.na(EXC_CWF) & !is.na(GOOD) & !is.na(GOOD_CWF))

  # Calculate, save score ----
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

  # Run calculation
  brc_score <- df_cwf %>%
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

  save(brc_score, file=file.path(output_path, 'brc_score.rda'))
}
