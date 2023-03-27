################################### HEADER ###################################
#  TITLE: mod_brcvar.R
#  DESCRIPTION: Select BRC sampling site, metrics, date range
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-27
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt)  x86_64
##############################################################################.

# library(shinyWidgets)
# library(lubridate)

########################################################################.
###                       User Interface                            ####
########################################################################.

BRCVAR_UI <- function(id, brc_data_num, brc_data_text, brc_data_score) {

  ns <- NS(id)

  # List parameters ----
  parList_score <- sort(unique(brc_data_score$PARAMETER))
  parList_graph <- sort(c('Water Depth', parList_score))
  parList_all <- sort(c(parList_graph, unique(brc_data_text$PARAMETER)))

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
    # Map, report card, download
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

    # Graphs (max 3 sites)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "graphs"'),
      pickerInput(
        ns('siteSelect_graph'),
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
    # Map (single selection)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "map"'),
      pickerInput(
        ns('catSelect_map'),
        label = h3('Select Parameter'),
        choices = parList_score,
        selected = parList_score[1],
        options = list(`live-search` = TRUE),
        multiple = FALSE
      )
    ),

    # Report Card (multiple selections)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "reportCard"'),
      pickerInput(
        ns('catSelect_reportCard'),
        label = h3('Select Parameters'),
        choices = parList_score,
        selected = parList_score,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE),
        multiple = TRUE
      )
    ),

    # Graph (single selection)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "graphs"'),
      pickerInput(
        ns('catSelect_graph'),
        label = h3('Select Parameter'),
        choices = parList_graph,
        selected = parList_graph[1],
        options = list(`live-search` = TRUE),
        multiple = FALSE
      )
    ),

    # Download (multiple selections)
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "download"'),
      pickerInput(
        ns('catSelect_download'),
        label = h3('Select Parameters'),
        choices = parList_all,
        selected = parList_all,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE),
        multiple = TRUE
      )
    ),

    # * Select date range ----
    conditionalPanel(
      condition = paste0('output["', ns('tabName'), '"] == "graphs" |
                           output["', ns('tabName'), '"] == "download"'),
      dateRangeInput(ns('dateRange'),
                     label = h3('Select Date Range'),
                     start = min(brc_data_num$DATE_TIME),
                     end = max(brc_data_num$DATE_TIME),
                     min = min(brc_data_num$DATE_TIME),
                     max = max(brc_data_num$DATE_TIME),
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
        choices = sort(unique(brc_data_score$YEAR), decreasing = TRUE),
        selected = max(brc_data_score$YEAR),
        multiple = FALSE)
    )
  )

}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

BRCVAR_SERVER <- function(id, brc_sites, brc_parameters, brc_data_score,
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
        filter(brc_sites, HUC12_NAME %in% input$riverSelect)
      } else {
        filter(brc_sites, TOWN %in% input$townSelect)
      }
    })

    observeEvent(riverTown(), {
      all_sites <- sort(unique(riverTown()$SITE_NAME))
      updatePickerInput(session = session,
                        inputId = "siteSelect",
                        choices = all_sites,
                        selected = all_sites)
      updatePickerInput(session = session,
                        inputId = "siteSelect_graph",
                        choices = all_sites,
                        selected = all_sites[1])
    })

    # Filter sites by watershed ----
    riverSelect <- reactive({
      filter(brc_sites, HUC12_NAME %in% input$riverSelect)
    })

    observeEvent(riverSelect(), {
      all_sites <- sort(unique(riverSelect()$SITE_NAME))
      updatePickerInput(session = session,
                        inputId = "siteSelect",
                        choices = all_sites,
                        selected = all_sites)
      updatePickerInput(session = session,
                        inputId = "siteSelect_graph",
                        choices = all_sites,
                        selected = all_sites[1])
    })

    # Filter sites by town ----
    townSelect <- reactive({
      filter(brc_sites, TOWN %in% input$townSelect)
    })

    observeEvent(townSelect(), {
      all_sites <- sort(unique(townSelect()$SITE_NAME))
      updatePickerInput(session = session,
                        inputId = "siteSelect",
                        choices = all_sites,
                        selected = all_sites)
      updatePickerInput(session = session,
                        inputId = "siteSelect_graph",
                        choices = all_sites,
                        selected = all_sites[1])
    })

    # DATAFRAMES ----
    # * Filter site data for selected sites ----
    df_site <- reactive({
      # required inputs
      req(input$siteSelect)

      df_site <- brc_sites %>%
        filter(SITE_NAME %in% input$siteSelect)

      return(df_site)
    })

    # * Filter scores for selected year ----
    df_data_score <- reactive({
      req(input$yearSelect)

      df_score <- brc_data_score %>%
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
        siteSelect_graph = reactive({ input$siteSelect_graph }),
        catSelect_map = reactive({ input$catSelect_map }),
        catSelect_reportCard = reactive({ input$catSelect_reportCard }),
        catSelect_graph = reactive({ input$catSelect_graph }),
        catSelect_download = reactive({ input$catSelect_download }),
        dateRange = reactive({ input$dateRange }),
        yearSelect = reactive({ input$yearSelect }),

        df_site = reactive({ df_site() }),
        df_data_score = reactive({ df_data_score() })
      )
    )

  })
}

# end Server Function
