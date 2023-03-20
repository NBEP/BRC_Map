################################### HEADER ###################################
#  TITLE: fun_brc_parameters.R
#  DESCRIPTION: Formats parameter data for BRC_Map R Shiny app
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-20
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(tidyverse)

# Parameter data ----

brcmap_format_parameters <- function(parameters_db, output_path){

  brc_parameters <- readRDS(parameters_db)

  save(brc_parameters, file=file.path(output_path, 'brc_parameters.rda'))

}
