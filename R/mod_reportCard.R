################################### HEADER ###################################
#  TITLE: mod_reportCard.R
#  DESCRIPTION: A module that generates a report card for BRC water quality data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-27
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(DT)
library(rmarkdown)
library(knitr)

########################################################################.
###                       User Interface                            ####
########################################################################.
REPORTCARD_UI <- function(id) {

  ns <- NS(id)

  tagList(
    htmlOutput(ns('title')),
    downloadButton(ns('downloadReport'), 'Download PDF'),
    tags$p(),
    DT::dataTableOutput(ns('reportCard'))
  )
}

########################################################################.
###                       Server Function                           ####
########################################################################.

REPORTCARD_SERVER <- function(id, brcvar) {
  moduleServer(
    id,
    function(input, output, session) {

      # Title (render text) ----
      output$title <- renderUI({
        HTML(paste0('<h2>Blackstone River Report Card (', brcvar$yearSelect(),
                   ')</h2>'))
      })

      # Calc site scores -----

      # * Filter scores for selected parameters ----
      df_score <- reactive({
        req(brcvar$catSelect_reportCard())

        df_score <- brcvar$df_data_score() %>%
          filter(PARAMETER %in% brcvar$catSelect_reportCard())

        return(df_score)
      })

      # * Join sites, scores ----
      df_site_score <- reactive({

        df_site <- brcvar$df_site()
        df_score <- df_score()

        # Generate list of all possible parameter/site combos
        df_param <- crossing('BRC_CODE' = unique(df_site$BRC_CODE),
                             'PARAMETER' = brcvar$catSelect_reportCard())

        # Merge df_site, df_score, df_param
        df_site_param <- merge(x=df_site, y=df_param, by='BRC_CODE')

        df_site_score <- merge(x=df_site_param, y=df_score,
                               by=c('BRC_CODE', 'PARAMETER'),
                               all.x=TRUE) %>%
          mutate_at('SCORE', replace_na, "No Data")

        return(df_site_score)
      })

      df_reportCard <- reactive({
        df_reportCard <- df_site_score() %>%
          # Combine TOWN, STATE in to single column (Town)
          unite('Town', TOWN:STATE, sep=', ') %>%
          # Rename columns
          rename(Site=SITE_NAME, 'Coldwater Fishery'=CFR,
                 Waterbody=WATERBODY_NAME, Watershed=HUC12_NAME,
                 Parameter=PARAMETER, Score=SCORE)  %>%
          # Arrange columns, drop any column not listed
          select(Site, Waterbody, 'Coldwater Fishery', Town, Watershed,
                 Parameter, Score) %>%
          # Sort
          arrange(Site, Parameter)
      })

      # Render table
      output$reportCard <- DT::renderDataTable({

        datatable(df_reportCard(), rownames = FALSE, filter='top') %>%
          # color code 'score' column
          formatStyle('Score',
                      backgroundColor = styleEqual(
                        c('Excellent', 'Good', 'Fair', 'Poor', 'No Data'),
                        c('#BBCCEE', '#CCEEFF', '#EEEEBB',
                          '#FFCCCC', '#FFFFFF')
                        )
          )
      })

      # Download PDF ----
      output$downloadReport <- downloadHandler(
        filename = 'brc-report.pdf',

        content = function(file) {
          src <- normalizePath(system.file("rmd", "brcReportCard.Rmd",
                                           package = "BRC_Map"))

          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          tempReport <- file.path(tempdir(), "brcReportCard.Rmd")
          file.copy(src, tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(df_report = df_reportCard(),
                         year = brcvar$yearSelect())

          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )

    })
} # end Server Function
