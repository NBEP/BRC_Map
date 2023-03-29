################################### HEADER ###################################
#  TITLE: mod_brcDownload.R
#  DESCRIPTION: A module that lets users download BRC water quality data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-29
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(writexl)
library(archive)

########################################################################.
###                       User Interface                            ####
########################################################################.
BRCDOWNLOAD_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h2('Suggested Citation'),
    h2('Download Data'),
    downloadButton(ns('dl_xls'), 'Download excel'),
    downloadButton(ns('dl_csv'), 'Download csv'),
    downloadButton(ns('dl_tsv'), 'Download tsv'),
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
          # Replace NA with -999999
          replace(is.na(.), -999999) %>%
          mutate(SITE_NUMBER = case_when(SITE_NUMBER=='NA' ~ '-999999',
                                         TRUE ~ SITE_NUMBER))
        
        return(df_site)
      })
      
      # * Numeric data ----
      df_data_num <- reactive({
        req(brcvar$dateRange())
        req(brcvar$catSelect_download())
        
        df_site <- df_site() %>%
          select(BRC_CODE, SITE_NAME)
        
        df_data <- brc_data_num %>%
          # Filter for selected parameters, date range
          filter(PARAMETER %in% brcvar$catSelect_download()) %>%
          filter((DATE_TIME >= brcvar$dateRange()[1]) &
                   (DATE_TIME <= brcvar$dateRange()[2]))
        
        # Merge data
        df_merge <- merge(x=df_site, y=df_data, by='BRC_CODE') %>%
          # Reorder columns
          relocate(SITE_NAME, .after = BRC_CODE) %>%
          # Sort
          arrange(DATE_TIME, BRC_CODE, PARAMETER) %>%
          # Change date format
          mutate(DATE_TIME = format(DATE_TIME, format="%Y-%m-%d %H:%M %Z")) %>%
          mutate_at("DATE_TIME", as.character)
        
        return(df_merge)
      })
      
      # * Qualitative data ----
      df_data_text <- reactive({
        req(brcvar$dateRange())
        req(brcvar$catSelect_download())
        
        df_site <- df_site() %>%
          select(BRC_CODE, SITE_NAME)
        
        df_data <- brc_data_text %>%
          # Filter for selected parameters, date range
          filter(PARAMETER %in% brcvar$catSelect_download()) %>%
          filter((DATE_TIME >= brcvar$dateRange()[1]) &
                   (DATE_TIME <= brcvar$dateRange()[2]))
        
        # Merge data
        df_merge <- merge(x=df_site, y=df_data, by='BRC_CODE') %>%
          # Reorder columns
          relocate(SITE_NAME, .after = BRC_CODE) %>%
          # Sort
          arrange(DATE_TIME, BRC_CODE, PARAMETER) %>%
          # Change date format
          mutate(DATE_TIME = format(DATE_TIME, format="%Y-%m-%d %H:%M %Z")) %>%
          mutate_at("DATE_TIME", as.character)
        
        return(df_merge)
      })
      
      # List data
      
      data_list <- reactive({
        data_list <- list(blackstone_sites = df_site())
        
        if(nrow(df_data_num())>0){
          data_list <- append(data_list, 
                              list(blackstone_data_num = df_data_num()))
        }
        
        if(nrow(df_data_text())>0){
          data_list <- append(data_list, 
                              list(blackstone_data_text = df_data_text()))
        }
        
        return(data_list)
      })
      
      # Download excel ----
      
      output$dl_xls <- downloadHandler(
        filename = function() { "brc_data.xlsx"},
        content = function(file) {write_xlsx(data_list(), path = file)}
      )
      
      # Download csv ----
      # Code based on stackoverflow answer by yben at 
      # https://stackoverflow.com/questions/43916535/
      
      output$dl_csv <- downloadHandler(
        filename = function() {"brc_data.zip"},
        
        content = function(file) {
          
          # temp dir for the csv's as we can only create
          # an archive from existent files and not data from R
          twd <- setwd(tempdir())
          on.exit(setwd(twd))
          files <- NULL
          
          # loop on data to download and write individual csv's
          for (i in 1:length(data_list())) {
            fileName <- paste0(names(data_list())[i], ".csv") # csv file name
            write.csv(data_list()[[i]], fileName, 
                      row.names=FALSE) # write csv in temp dir
            files <- c(files, fileName) # store written file name
          }
          
          # create archive from written files
          archive_write_files(file, files)
        }
      )
      
      # Download tsv ----
      output$dl_tsv <- downloadHandler(
        filename = function() {"brc_data.zip"},
        
        content = function(file) {
          
          # temp dir for the tsv's as we can only create
          # an archive from existent files and not data from R
          twd <- setwd(tempdir())
          on.exit(setwd(twd))
          files <- NULL
          
          # loop on data to download and write individual tsv's
          for (i in 1:length(data_list())) {
            fileName <- paste0(names(data_list())[i], ".tsv") # tsv file name
            write.table(data_list()[[i]], fileName, 
                        sep='\t', row.names = FALSE) # write tsv in temp dir
            files <- c(files, fileName) # store written file name
          }
          
          # create archive from written files
          archive_write_files(file, files)
        }
      )
      
    })
} # end Server Function
