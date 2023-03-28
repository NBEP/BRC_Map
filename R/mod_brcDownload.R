################################### HEADER ###################################
#  TITLE: mod_brcDownload.R
#  DESCRIPTION: A module that lets users download BRC water quality data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-02
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(writexl)

########################################################################.
###                       User Interface                            ####
########################################################################.
BRCDOWNLOAD_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h2('Download Data'),
    h3('Site Data'),
    downloadButton(ns('dl_site_csv'), 'Download csv'),
    downloadButton(ns('dl_site_xls'), 'Download excel'),
    h3('Parameter Data'),
    downloadButton(ns('dl_data_csv'), 'Download csv'),
    downloadButton(ns('dl_data_xls'), 'Download excel'),
  ) 
}

########################################################################.
###                       Server Function                           ####
########################################################################.

BRCDOWNLOAD_SERVER <- function(id, brc_data_num, brc_data_text, brcvar) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Format data ----
      # * Site data ----
      df_site <- reactive({
        
        df_site <- brcvar$df_site() %>%
          # Drop extra columns
          select('SITE_NUMBER', 'BRC_CODE', 'SITE_NAME', 'WATERBODY_NAME', 
                 'ZONE', 'FISHERY', 'LATITUDE', 'LONGITUDE', 'TOWN', 'STATE',
                 'HUC12_NUM', 'HUC12_NAME')
        
        return(df_site)
      })
      
      # * Parameter data ----
      df_data <- reactive({
        
        df_site <- df_site() %>%
          select(BRC_CODE, SITE_NAME)
        
        df_data <- brc_data_num
        
        # Merge data
        df_merge <- merge(x=df_site, y=df_data, by='BRC_CODE') %>%
          # Drop extra columns
          select('BRC_CODE', 'SITE_NAME', 'DATE_TIME', 'PARAMETER', 'RESULT', 
                 'UNITS', 'UNIQUE_ID')
        
        return(df_merge)
      })
      
      # Download csv ----
      # * Sites ----
      output$dl_site_csv <- downloadHandler(
        filename = function() {"brc_sites.csv"},
        content = function(file) {
          write.csv(df_site(), file, row.names = FALSE)
        }
      )
      
      output$dl_site_xls <- downloadHandler(
        filename = function() { "brc_sites.xlsx"},
        content = function(file) {write_xlsx(df_site(), path = file)}
      )
        
      # * Param ----
      output$dl_data_csv <- downloadHandler(
        filename = function() {"brc_data.csv"},
        content = function(file) {
          write.csv(df_data(), file, row.names = FALSE)
        }
      )
      
      output$dl_data_xls <- downloadHandler(
        filename = function() { "brc_data.xlsx"},
        content = function(file) {write_xlsx(df_data(), path = file)}
      )
      
    })
} # end Server Function
