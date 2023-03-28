################################### HEADER ###################################
#  TITLE: app.R
#  DESCRIPTION: R shiny app for displaying BRC water quality data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-28
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt)  x86_64
##############################################################################.

library(shiny)
library(tidyverse)

# For testing
library(shinya11y)

BRC_Map <- function(...){

  ########################################################################.
  #                         User Interface                               #
  ########################################################################.

  ui <- fluidPage(
    #theme = bslib::bs_theme(bootswatch = 'sandstone'),
    #use_tota11y(),

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
    brcvar <- BRCVAR_SERVER('brcvar', brc_sites, brc_parameters, brc_data_score, 
                            reactive({ input$tabset }))
    BRCMAP_SERVER('brcmap', brcvar)
    REPORTCARD_SERVER('reportCard', brcvar)
    BRCGRAPHS_SERVER('brcgraphs', brc_sites, brc_parameters, brc_data_num, 
                     brcvar)
    BRCDOWNLOAD_SERVER('brcdownload', brc_data_num, brc_data_text, brcvar)
  }



  ########################################################################.
  #                             Run App                                  #
  ########################################################################.

  shinyApp(ui, server)
}

