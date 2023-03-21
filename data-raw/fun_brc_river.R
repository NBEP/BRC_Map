################################### HEADER ###################################
#  TITLE: fun_brc_sites.R
#  DESCRIPTION: Adds river shapefile to app
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-21
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(sf)
library(tidyverse)

shp_river <- read_sf(dsn = "data-raw", layer = "BLACKSTONE_RIVERS_NBEP2023") %>%
  mutate(gnis_name=replace_na(gnis_name, 'Unnamed Waterbody'))

usethis::use_data(shp_river, overwrite=TRUE)
