################################### HEADER ###################################
#  TITLE: mod_brcvar.R
#  DESCRIPTION: Select BRC sampling site, metrics, date range
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-02-16
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)
library(tidyverse)
library(lubridate)

########################################################################.
###                       User Interface                            ####
########################################################################.

BRCVAR_UI <- function(id, data_db, data_score_db) {
  
  ns <- NS(id)
  
  # List parameters (all) ----
  parList <- sort(unique(data_db$PARAMETER))
  
  # * List parameters (short) ---- 
  parList_short <- parList %>%
    # Drop all replicates, blanks
    purrr::discard(.p = ~stringr::str_detect(.x,"Replicate")) %>%
    purrr::discard(.p = ~stringr::str_detect(.x,"Blank")) %>%
    # Drop specific items
    purrr::discard(.p = ~stringr::str_detect(.x,"Air Temperature"))
  
  # List watersheds -----
  riverList <- c('Abbott Run','Branch River', 'Chepachet River', 
                 'Clear River', 'Emerson Brook-Blackstone River',
                 'Kettle Brook', 'Mill River', 'Mumford River',
                 'Peters River-Blackstone River', 'Quinsigamond River',
                 'Singletary Brook-Blackstone River', 
                 'Tatnuck Brook-Blackstone River', 'West River')
  
  tagList(
    # Select data ----
    h2('Select Data'),
    # * Filter by river OR filter by town ----
    radioGroupButtons(ns("riverTown"), 
                      label = h3('Select Location'),
                      choices = c('Select Watershed' = 'river', 
                                  'Select Town' = 'town'),
                      justified = FALSE),
    
    # * Select rivers ----
    # Only show if "rivers" selected in river/town toggle
    conditionalPanel(
      condition = paste0('input["', ns('riverTown'), '"] == "river"'),
      
      pickerInput(
        ns("riverSelect"),
        label = h4('Select Watersheds'), 
        choices = riverList,
        selected = riverList,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE), 
        multiple = TRUE)
    ),
    
    # * Select towns ----
    # Only show if "towns" selected in river/town toggle
    conditionalPanel(
      condition = paste0('input["', ns('riverTown'), '"] == "town"'),
      pickerInput(
        ns("townSelect"),
        label = h4('Select Towns'), 
        choices = c('Attleboro, MA' = 'Attleboro', 'Auburn, MA' = 'Auburn',
                    'Bellingham, MA' = 'Bellingham', 'Blackstone, MA' = 
                      'Blackstone', 'Boylston, MA' = 'Boylston', 
                    'Burrillville, RI' = 'Burrillville', 'Central Falls, RI' =
                      'Central Falls', 'Cumberland, RI' = 'Cumberland', 
                    'Douglas, MA' = 'Douglas', 'Franklin, MA' = 'Franklin', 
                    'Glocester, RI' = 'Glocester', 'Grafton, MA' = 'Grafton',
                    'Holden, MA' = 'Holden', 'Hopedale, MA' = 'Hopedale', 
                    'Leicester, MA' = 'Leicester', 'Lincoln, RI' = 'Lincoln',
                    'Mendon, MA' = 'Mendon', 'Milford, MA' = 'Milford', 
                    'Millbury, MA' = 'Millbury', 'Millville, MA' = 'Millville',
                    'North Attleborough, MA' = 'North Attleborough', 
                    'North Smithfield, RI' = 'North Smithfield',
                    'Northbridge, MA' = 'Northbridge', 
                    'Oxford, MA' = 'Oxford', 'Pawtucket, RI' = 'Pawtucket', 
                    'Paxton, MA' = 'Paxton', 'Plainville, MA' = 'Plainville',
                    'Scituate, RI' = 'Scituate', 'Shrewsbury, MA' = 
                      'Shrewsbury', 'Smithfield, MA' = 'Smithfield', 
                    'Sutton, MA' = 'Sutton', 'Upton, MA' = 'Upton', 
                    'Uxbridge, MA' = 'Uxbridge', 'Webster, MA' = 'Webster',
                    'West Boylston, MA' = 'West Boylston', 'Westborough, MA' =
                      'Westborough', 'Woonsocket, RI' = 'Woonsocket', 
                    'Worcester, MA' = 'Worcester', 'Wrentham, MA' = 'Wrentham'),
        selected = c('Attleboro', 'Auburn', 'Bellingham', 'Blackstone',
                     'Boylston', 'Burrillville', 'Central Falls', 
                     'Cumberland', 'Douglas', 'Franklin', 'Glocester', 
                     'Grafton', 'Holden', 'Hopedale', 'Hopkinton', 
                     'Leicester', 'Lincoln', 'Mendon', 'Milford', 'Millbury',
                     'Millville', 'North Attleborough', 'North Smithfield',
                     'Northbridge', 'Oxford', 'Pawtucket', 'Paxton', 
                     'Plainville', 'Scituate', 'Shrewsbury', 'Smithfield',
                     'Sutton', 'Upton', 'Uxbridge', 'Webster', 
                     'West Boylston', 'Westborough', 'Woonsocket', 
                     'Worcester', 'Wrentham'),
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE), 
        multiple = TRUE)
    ),
    
    # * Select sites ----
    # Unlimited
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] != "graphs"'),
      pickerInput(
        ns('siteSelect'),
        label = h4('Select Sampling Locations'), 
        choices = NULL,
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE), 
        multiple = TRUE)
    ),
    
    # Limited
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "graphs"'),
      pickerInput(
        ns('siteSelect_1'),
        label = HTML(paste(h4('Select Sampling Locations'), 
                           'Select up to three sites')), 
        choices = NULL,
        selected = NULL,
        options = list(
          `live-search` = TRUE,
          `max-options` = 3), 
        multiple = TRUE)
    ),
    
    # * Select parameters ----
    # Multiple selections (Report Card)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "reportCard"'),
      pickerInput(
        ns('catSelect'),
        label = h3('Select Parameters'),
        choices = parList_short,
        selected = parList_short,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE),
        multiple = TRUE
      )
    ),
    
    # Multiple selections (Download)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "download"'),
      pickerInput(
        ns('catSelect_long'),
        label = h3('Select Parameters'),
        choices = parList,
        selected = parList,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE),
        multiple = TRUE
      )
    ),
    
    # Single selection (Graphs, Map)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "graphs" | 
                         output["', ns('tabName'), '"] == "map"'),
      pickerInput(
        ns('catSelect_1'),
        label = h3('Select Parameter'),
        choices = parList_short,
        selected = parList_short[1],
        options = list(`live-search` = TRUE),
        multiple = FALSE
      )
    ),
    
    # * Select date range ----
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "graphs" | 
                           output["', ns('tabName'), '"] == "download"'),
      dateRangeInput(ns('dateRange'),
                     label = h3('Select Date Range'),
                     start = min(data_db$DATE_TIME), 
                     end = max(data_db$DATE_TIME),
                     min = min(data_db$DATE_TIME),
                     max = max(data_db$DATE_TIME),
                     separator = " - ", format = "mm/dd/yy",
                     startview = 'year', language = 'eng', weekstart = 1
      )
    ),
    
    # * Select year ----
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "map" | 
                           output["', ns('tabName'), '"] == "reportCard"'),
      pickerInput(
        ns('yearSelect'),
        label = h3('Select Year'), 
        choices = sort(unique(data_score_db$YEAR), decreasing = TRUE),
        selected = max(data_score_db$YEAR), 
        multiple = FALSE)
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

BRCVAR_SERVER <- function(id, sites_db, parameters_db, data_db, data_score_db,
                          tabSelect) {
  moduleServer(id, function(input, output, session) {
    
    
    # Check tabs ----
    output$tabName <- renderText({
      tabSelect()
    })
    
    outputOptions(output, "tabName", suspendWhenHidden = FALSE)
    
    # Reset sites when toggle between rivers, towns ----
    riverTown <- reactive({
      if (input$riverTown == 'river') {
        filter(sites_db, HUC12_NAME %in% input$riverSelect)
      } else {
        filter(sites_db, TOWN %in% input$townSelect)
      }
    })
    
    observeEvent(riverTown(), {
      all_sites <- sort(unique(riverTown()$SITE_NAME))
      updatePickerInput(session = session,
                        inputId = "siteSelect", 
                        choices = all_sites,
                        selected = all_sites)
      updatePickerInput(session = session,
                        inputId = "siteSelect_1", 
                        choices = all_sites,
                        selected = all_sites[1])
    })
    
    # Filter sites by watershed ----
    riverSelect <- reactive({
      filter(sites_db, HUC12_NAME %in% input$riverSelect)
    })
    
    observeEvent(riverSelect(), {
      all_sites <- sort(unique(riverSelect()$SITE_NAME))
      updatePickerInput(session = session,
                        inputId = "siteSelect", 
                        choices = all_sites,
                        selected = all_sites)
      updatePickerInput(session = session,
                        inputId = "siteSelect_1", 
                        choices = all_sites,
                        selected = all_sites[1])
    })
    
    # Filter sites by town ----
    townSelect <- reactive({
      filter(sites_db, TOWN %in% input$townSelect)
    })
    
    observeEvent(townSelect(), {
      all_sites <- sort(unique(townSelect()$SITE_NAME))
      updatePickerInput(session = session,
                        inputId = "siteSelect", 
                        choices = all_sites,
                        selected = all_sites)
      updatePickerInput(session = session,
                        inputId = "siteSelect_1", 
                        choices = all_sites,
                        selected = all_sites[1])
    })
    
    # DATAFRAMES ----
    # * Filter site data for selected sites ----
    df_site <- reactive({
      # required inputs
      req(input$siteSelect)
      
      df_site <- sites_db %>%
        filter(SITE_NAME %in% input$siteSelect)
      
      return(df_site)
    })
    
    # * Filter data for selected date range ----
    df_data <- reactive({
      # required inputs
      req(input$dateRange)
      
      df_data <- data_db %>%
        # Between leads to null values because ????
        filter((DATE_TIME >= input$dateRange[1]) & 
                 (DATE_TIME <= input$dateRange[2]))
      
      return(df_data)
    })
    
    # * Filter scores for year ----
    df_score <- reactive({
      req(input$yearSelect)
      
      df_score <- data_score_db %>%
        filter(YEAR == input$yearSelect)
      
      return(df_score)
    })
    
    # Output reactive values ----
    return(
      list(
        riverTown = reactive({ input$riverTown }),
        riverSelect = reactive({ input$riverSelect }),
        townSelect = reactive({ input$townSelect }),
        siteSelect = reactive({ input$siteSelect }),
        siteSelect_1 = reactive({ input$siteSelect_1 }),
        catSelect = reactive({ input$catSelect }),
        catSelect_long = reactive({ input$catSelect_long }),
        catSelect_1 = reactive({ input$catSelect_1 }),
        dateRange = reactive({ input$dateRange }),
        yearSelect = reactive({ input$yearSelect }),
        
        df_site = reactive({ df_site() }),
        df_data = reactive({ df_data() }),
        df_score = reactive({ df_score() })
      )
    )
    
  })
}

# end Server Function
