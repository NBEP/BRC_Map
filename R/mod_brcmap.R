################################### HEADER ###################################
#  TITLE: app.R
#  DESCRIPTION: R shiny app for displaying BRC water quality data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-27
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt)  x86_64
##############################################################################.

# For testing purposes
library(shinya11y)

BRC_Map <- function(...){
  # Data ----
  sites_db <- sites_db %>%
    mutate(SITE_NAME = make.unique(SITE_NAME, sep=" ")) %>%
    mutate(FISHERY = case_when(CFR=='Yes' ~ 'Coldwater',
                               CFR=='No' ~ 'Warmwater'))

  parameters_db <- parameters_db %>%
    filter(!is.na(AVG) & !is.na(AVG_CWF) & !is.na(EXC) & !is.na(GOOD))

  data_db <- data_db %>%
    # Remove null values
    filter(RESULT != -999999) %>%
    # Set DATE_TIME to datetime format
    mutate(DATE_TIME = ymd_hm(DATE_TIME, tz = "America/New_York")) %>%
    # Calc year
    mutate(YEAR = lubridate::year(DATE_TIME)) %>%
    # Rename columns
    rename(BRC_CODE = SITE_BRC_CODE)

  ########################################################################.
  #                         User Interface                               #
  ########################################################################.

  ui <- fluidPage(
    #theme = bslib::bs_theme(bootswatch = 'sandstone'),
    use_tota11y(),

    tags$header(
      class = "col-sm-12 title-panel",
      tags$h1("Blackstone River Coalition Water Quality Data")
    ),
    sidebarLayout(

      # * Side panel ----
      sidebarPanel(
        # Module: select variables
        BRCVAR_UI('brcvar', brc_data_num, brc_data_text, brc_data_score)

      ),

      # * Main panel ----
      mainPanel(
        tabsetPanel(type = 'tabs',
                    id = 'tabset',
                    # Tab: Main page ----
                    tabPanel('Map',
                             value='map',
                             BRCMAP_UI('brcmap')
                    ),

                    # Tab: Report Card ----
                    tabPanel('Report Card',
                             value='reportCard',
                             REPORTCARD_UI('reportCard')
                    ),

                    # Tab: Graphs ----
                    tabPanel('Graphs',
                             value='graphs',
                             BRCGRAPHS_UI('brcgraphs')
                    ),

                    # Tab: Download ----
                    tabPanel('Download Data',
                             value='download',
                             BRCDOWNLOAD_UI('brcdownload')
                    )
        )

      )
    )
  )

  # Set language -----
  attr(ui, "lang")="en"

  ########################################################################.
  #                         Server Function                              #
  ########################################################################.
  server <- function(input, output, session) {
    # Add module servers ----
    brcvar <- BRCVAR_SERVER('brcvar', brc_sites, brc_parameters, brc_data_num,
                            brc_data_score, reactive({ input$tabset }))
    BRCMAP_SERVER('brcmap', brcvar)
    REPORTCARD_SERVER('reportCard', brcvar)
    BRCGRAPHS_SERVER('brcgraphs', sites_db, parameters_db, brcvar)
    BRCDOWNLOAD_SERVER('brcdownload', brcvar)
  }



  ########################################################################.
  #                             Run App                                  #
  ########################################################################.

  shinyApp(ui, server)
}

