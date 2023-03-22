################################### HEADER ###################################
#  TITLE: fun_brc_sites.R
#  DESCRIPTION: Formats site data for BRC_Map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-22
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(tidyverse)

# Site data -----

brcmap_format_sites <- function(sites_db, output_path){

  RI_towns = c('Burrillville', 'Central Falls', 'Cumberland', 'Glocester',
               'Lincoln', 'North Smithfield', 'Pawtucket', 'Scituate',
               'Woonsocket')

  brc_sites <- readRDS(sites_db) %>%
    # Make site names unique
    mutate(SITE_NAME = make.unique(SITE_NAME, sep=" ")) %>%
    # Add STATE column
    mutate(STATE = case_when(TOWN %in% RI_towns ~ 'RI',
                             !(TOWN %in% RI_towns) ~ 'MA')) %>%
    # Select columns
    select(SITE_NUMBER, BRC_CODE, SITE_NAME, LATITUDE, LONGITUDE,
           WATERBODY_NAME, ZONE, CFR, TOWN, STATE, HUC12_NUM, HUC_NAME,
           CONDUCTIVITY_USCM, WATER_DEPTH_FT) %>%
    # Tweak column names
    rename(HUC12_NAME=HUC_NAME) %>%
    # Tweak HUC12_NUM, ensure starts with zero
    mutate(HUC12_NUM=ifelse(nchar(HUC12_NUM)<12, paste0("0", HUC12_NUM),
                            HUC12_NUM))

  # Save as rda
  save(brc_sites, file=file.path(output_path, 'brc_sites.rda'))
}

brcmap_format_sites('data-raw/sites_db.rds', 'data')
