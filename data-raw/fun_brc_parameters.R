################################### HEADER ###################################
#  TITLE: fun_brc_parameters.R
#  DESCRIPTION: Formats parameter data for BRC_Map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-22
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(tidyverse)

# Parameter data ----

brcmap_format_parameters <- function(parameters_db, output_path){

  brc_parameters <- readRDS(parameters_db) %>%
    # Drop extra columns
    select(PARAMETER_NAME, UNITS, PARAMETER_DESCR, CATEGORY, SCORE, EXC,
           EXC_CWF, GOOD, GOOD_CWF, FAIR, FAIR_CWF, AVG, AVG_CWF) %>%
    # Drop extra rows
    filter(CATEGORY != 'People')

  save(brc_parameters, file=file.path(output_path, 'brc_parameters.rda'))

}

brcmap_format_parameters('data-raw/parameters_db.rds', 'data')
