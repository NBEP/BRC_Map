################################### HEADER ###################################
#  TITLE: mod_brcGraphs.R
#  DESCRIPTION: A module that generates graphs for BRC water quality data
#  AUTHOR(S): Mariel Sorlien, Dan Crocker
#  DATE LAST UPDATED: 2023-02-22
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library('tidyverse')
library('glue')
library('highcharter')

########################################################################.
#                              Style                                ####
########################################################################.

# Graph theme ----
brc_theme <- hc_theme(
  colors = c('#332288',  '#44AA99', '#117733', '#999933', '#88CCEE', '#DDCC77'),
  chart = list(backgroundColor='#FFFFFF'),
  plotOptions = list(
    series = list(
      marker = list(enabled=TRUE,
                    radius=4),
      showInLegend = TRUE,
      extend=TRUE,
      connectNulls=FALSE
    )
  )
)

########################################################################.
#                         User Interface                            ####
########################################################################.
BRCGRAPHS_UI <- function(id) {

  ns <- NS(id)

  tagList(

    # Layout ----

    # * Site header, table ----
    h2('Site Summary'),

    tableOutput(ns('site_table')),

    # * Data header, table/graph ----

    # Heading
    htmlOutput(ns('param_header')),

    # Display graph or table
    radioGroupButtons(ns('plot_table'),
                      label = 'View as',
                      choices = c('Graph', 'Table'),
                      justified = FALSE),

    # Graph
    conditionalPanel(
      paste0('input["', ns('plot_table'), '"] == "Graph"'),
      highchartOutput(ns("param_plot"))
    ),

    # Table
    conditionalPanel(
      paste0('input["', ns('plot_table'), '"] == "Table"'),
      DT::dataTableOutput(ns("param_table"))
    )
  )
}

########################################################################.
#                         Server Function                           ####
########################################################################.

BRCGRAPHS_SERVER <- function(id, sites_db, parameters_db, choice) {
  moduleServer(
    id,
    function(input, output, session) {

      # Filter data ----
      # * Sites ----
      df_site <- reactive({
        df_site <- sites_db %>%
          filter(SITE_NAME %in% choice$siteSelect_1())

        return(df_site)
      })

      # * Data ----
      df_data <- reactive({
        req(choice$catSelect_1())

        df_data <- choice$df_data() %>%
          filter(PARAMETER == choice$catSelect_1())

        return(df_data)
      })

      # * Parameters -----

      df_param <- reactive({
        req(choice$catSelect_1())

        df_param <- parameters_db %>%
          filter(PARAMETER_NAME == choice$catSelect_1())
      })

      # Join sites, data ----
      df_merge <- reactive({

        df_site <- df_site() %>%
          select(BRC_CODE, SITE_NAME, CFR)

        df_data <- df_data()

        df_merge <- merge(x=df_site, y=df_data, by='BRC_CODE') %>%
          # Drop extra columns
          select(SITE_NAME, CFR, DATE_TIME, YEAR, PARAMETER, RESULT, UNITS) %>%
          # Convert datetime to date
          mutate(DATE_TIME = as.Date(DATE_TIME))

        # Rename columns
        names(df_merge) <- c("Site", "CWF", "Date", "Year", "Parameter",
                             "Results", "Units")

        return(df_merge)
      })

      ########################################################################.
      #                                TEXT                               ####
      ########################################################################.

      # * Min/Max text
      param_minmax <- reactive({
        if(nrow(df_param()>0)) {

          if(df_param()$EXC > df_param()$GOOD){
            param_minmax <- 'lowest'
          } else {
            param_minmax <- 'highest'
          }
        } else {
          param_minmax <- ''
        }

        return(param_minmax)
      })

      # * Header: Parameter ----

      output$param_header <- renderUI({
        HTML(glue('<h2>{choice$catSelect_1()}</h2>'))
      })

      # * Caption ----

      param_caption <- reactive({

        # Define variables
        sites <- df_site()$SITE_NAME
        site_param <- c('X', 'Y', 'Z')

        param <- choice$catSelect_1()
        units <- df_data()$UNITS[1]

        # Generate sentence with min/max acceptable value for coldwater &
        #   warmwater fisheries
        if(nrow(df_param()>0)){
          param_wwf <- df_param()$AVG
          param_cwf <- df_param()$AVG_CWF

          if(param_wwf == param_cwf){
            param_sentence <- glue('The {param_minmax()} acceptable value is
                                   {param_wwf} {units}.')
          } else {
            param_sentence <- glue('The {param_minmax()} acceptable value is
            {param_wwf} {units} for warmwater fisheries and {param_cwf} {units}
            for coldwater fisheries.')
          }
        } else {
          param_sentence = ''
        }

        # Generate sentence fragment with list of sites & sentence fragment with
        #   baseline site data for conductivity, water depth

        if (length(sites) == 3){
          site_list <- glue('{sites[1]}, {sites[2]}, and {sites[3]}')
          value_list <- glue('{site_param[1]} {units} for {sites[1]},
          {site_param[2]} {units} for {sites[2]}, and {site_param[3]} {units}
                             for {sites[3]}')
        } else if (length(sites) == 2) {
          site_list <- glue('{sites[1]} and {sites[2]}')
          value_list <- glue('{site_param[1]} {units} for {sites[1]} and
          {site_param[2]} {units} for {sites[2]}')
        } else {
          site_list <- paste(sites[1])
          value_list <- glue('{site_param[1]} {units} for {sites[1]}')
        }

        # Assemble sentence with baseline site data for conductivity, water depth
        if (param %in% c('Conductivity', 'Water Depth')) {
          param_sentence <- glue('The baseline {tolower(param)} is
                                 {value_list}.')
        }

        # Assemble sentences to generate complete caption
        return(glue('{param} ({units}) for {site_list}. {param_sentence}'))

        })

      ########################################################################.
      #                               TABLES                              ####
      ########################################################################.

      # * Table: Sites ----
      output$site_table <- renderTable({

        df_site <- df_site() %>%
          unite('Town', TOWN:STATE, sep=', ') %>%
          # Rename columns
          rename(Site=SITE_NAME, Waterbody=WATERBODY_NAME, Fishery=FISHERY,
                 Watershed=HUC12_NAME)  %>%
          # Arrange columns, drop any column not listed
          select(Site, Waterbody, Fishery, Town, Watershed)

        return(df_site)

        },
        hover = TRUE, striped = TRUE
        )

      # * Table: Data ----
      output$param_table <- DT::renderDataTable({

        df_long <- df_merge() %>%
          select(Date, Site, Results)

        df_wide <- spread(df_long, Site, Results)

        datatable(df_wide, rownames = FALSE,
                  caption = param_caption()
        )

      })

      ########################################################################.
      #                                 PLOT                              ####
      ########################################################################.

      # Prep data ----

      df_graph <- reactive({

        if(nrow(df_merge())>0){
          # Add a Jan 1 null Results value for each site/year combo
          # (Ensures lines don't connect across years)
          sites_i <- unique(df_merge()$Site)
          years_i <- unique(df_merge()$Year)

          df_null <- crossing(sites_i, years_i) %>%
            rename(Site=sites_i, Year=years_i) %>%
            mutate(Date=as.Date(paste0(Year,'-1-1')))

          df_bind <- bind_rows(df_merge(), df_null)

          # Sort by date
          df_bind <- df_bind %>%
            arrange(Date)

          return(df_bind)
        } else {
          return(NULL)
        }

      })

      # Make graph ----
      output$param_plot <- renderHighchart({
        if (!is.null(df_graph())){
          return(hc_plot(df_graph()))
        } else {
          return(NULL)
        }

      })


      # Function: Create graph ----
      hc_plot <- function(df) {

        # Define variables ----
        par <- choice$catSelect_1()
        unit <- df_data()$UNITS[1]
        min_par <- min(df_data()$RESULT)
        max_par <- max(df_data()$RESULT)

        df_param <- df_param()

        # Define Y-axis
        ylab <- glue("{par} ({unit})")

        # * Plotlines, plotbands ----

        # Set default null value for plotlines, plotbands
        plotbands_yaxis = NULL
        plotlines_yaxis = NULL

        # Add plotlines, plotbands if data available
        if(nrow(df_param)>0) {
          par_cutoff = df_param$AVG
          par_cutoff_cwf = df_param$AVG_CWF

          # Set lower/upper range for plotbands
          if (df_param$EXC > df_param$GOOD) {
            # Set lower bound
            minmax_value = min(c(min_par, par_cutoff, par_cutoff_cwf,
                                 0))
          } else {
            # Set upper bound
            minmax_value = max(c(max_par, par_cutoff, par_cutoff_cwf,
                                 100))
          }

          # Creat plotbands, plotlines

          # Warmwater fishery
          plotbands_hot = list(
            from = par_cutoff,
            to = minmax_value,
            color='#FFCCCC'
          )

          plotlines_hot = list(
            value = par_cutoff,
            color = '#CC6677',
            width = 2,
            zIndex = 3
          )

          # Cold water fishery
          plotbands_cold = list(
            from = par_cutoff_cwf,
            to = minmax_value,
            color='#FFCCCC'
          )

          plotlines_cold = list(
            value = par_cutoff_cwf,
            color = '#CC6677',
            dashStyle = 'longdash',
            width = 2,
            zIndex = 3
          )

          # Check if cwf
          if ('Yes' %in% df$CWF & 'No' %in% df$CWF) {
            plotbands_yaxis = list(plotbands_hot, plotbands_cold)
            plotlines_yaxis = list(plotlines_hot, plotlines_cold)
          } else if ('Yes' %in% df$CWF) {
            plotbands_yaxis = list(plotbands_cold)
            plotlines_yaxis = list(plotlines_cold)
          } else {
            plotbands_yaxis = list(plotbands_hot)
            plotlines_yaxis = list(plotlines_hot)
          }

        } # End if nrow(df_param)>0

        # Create graph ----
        hc_graph <- highchart() %>%
          #hc_add_dependency(name = "modules/accessibility.js") %>%
          hc_add_dependency(name = "modules/exporting.js") %>%
          hc_add_dependency(name = "modules/export-data.js") %>%

          # * Add data ----
          hc_add_series(df,
                        type = 'line',
                        hcaes(x=Date, y=Results, group=Site)
          ) %>%

          # * Axes, plotlines, plotbands ----
          hc_xAxis(type='datetime',
                   title=list(text='Date')
          ) %>%
          hc_yAxis(opposite=FALSE,
                   title = list(text = ylab),
                   plotBands = plotbands_yaxis,
                   plotLines = plotlines_yaxis
          ) %>%

          # * Caption ----
          hc_caption(
            text = param_caption()
          ) %>%

          # * Custom zoom levels ----
          hc_rangeSelector(enabled = TRUE,
                           buttons = list(
                             list(type = 'year', count = 1, text = '1y',
                                  title = 'View 1 year'),
                             list(type = 'year', count = 5, text = '5y',
                                  title = 'View 5 years'),
                             list(type = 'all', text = 'All',
                                  title = 'View all'))
                           ) %>%
          hc_navigator(enabled=TRUE) %>%

          # * Legend, theme ----
          hc_legend(enabled=TRUE,
                  symbolWidth=40) %>%
          hc_add_theme(brc_theme) %>%

          # * Export data ----
          hc_exporting(enabled=TRUE,
                       accessibility = list(enabled = TRUE),
                       csv=list(dateFormat= '%Y-%m-%d'),
                       # Add title on export
                       chartOptions = list(
                         title = list(
                           text=par
                         )
                       ),
                       # Set custom download options
                       buttons = list(
                         contextButton = list(
                           menuItems = c("viewFullscreen",
                                         "printChart",
                                         "separator",
                                         "downloadPNG",
                                         "downloadJPEG",
                                         "downloadPDF",
                                         "downloadSVG",
                                         "separator",
                                         "downloadCSV",
                                         "downloadXLS")
                         )
                       )
                       ) %>%

          # * Accessibility ----
          hc_plotOptions(
            accessibility=list(
              enabled = TRUE,
              keyboardNavigation = list(enabled = TRUE)
            )
          )

      # return graph
      return(hc_graph)

    }

    })
} # end Server Function
