################################### HEADER ###################################
#  TITLE: mod_brcGraphs.R
#  DESCRIPTION: A module that generates graphs for BRC water quality data
#  AUTHOR(S): Mariel Sorlien, Dan Crocker
#  DATE LAST UPDATED: 2023-03-28
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

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

BRCGRAPHS_SERVER <- function(id, brc_sites, brc_parameters, brc_data_num,
                             brcvar) {
  moduleServer(
    id,
    function(input, output, session) {

      # Filter data ----
      # * Sites ----
      df_site <- reactive({
        df_site <- brc_sites %>%
          filter(SITE_NAME %in% brcvar$siteSelect_graph())

        return(df_site)
      })

      # * Parameters -----

      # Drop rows with null data (nonreactive)
      df_param <- brc_parameters %>%
        filter(!is.na(AVG) & !is.na(AVG_CWF) & !is.na(EXC) & !is.na(EXC_CWF))

      # Select data for selected parameter (reactive)
      df_param_filter <- reactive({
        req(brcvar$catSelect_graph())

        df_param_filter <- df_param %>%
          # Select data for selected parameter
          filter(PARAMETER_NAME == brcvar$catSelect_graph())
      })

      # * Data ----
      # Format data (nonreactive, run once)
      df_data <- brc_data_num %>%
        # Drop extra rows
        filter(SAMPLE_TYPE %in% c('Grab', 'Composite', 'Replicate'),
               RESULT != -999999) %>%
        # Convert datetime to date
        mutate(DATE = as.Date(DATE_TIME)) %>%
        # Add year
        mutate(YEAR = lubridate::year(DATE_TIME)) %>%
        # Sort by DATE, SAMPLE_TYPE
        arrange(DATE, SAMPLE_TYPE) %>%
        # Group data, take first entry (composite, otherwise grab/replicate)
        group_by(BRC_CODE, DATE, YEAR, PARAMETER, UNITS) %>%
        summarise(RESULT = first(RESULT),
                  SAMPLE_TYPE = first(SAMPLE_TYPE),
                  .groups = 'drop')

      # Filter for selected parameter, date range (reactive)
      df_data_filter <- reactive({
        req(brcvar$catSelect_graph())
        req(brcvar$dateRange())

        df_data_filter <- df_data %>%
          # Filter for parameter
          filter(PARAMETER == brcvar$catSelect_graph()) %>%
          # Filter for date
          filter((DATE >= brcvar$dateRange()[1]) &
                   (DATE <= brcvar$dateRange()[2]))

        return(df_data_filter)
      })

      # Join sites, data ----
      df_merge <- reactive({

        df_site <- df_site() %>%
          select(BRC_CODE, SITE_NAME, CFR)

        df_data <- df_data_filter()

        df_merge <- merge(x=df_site, y=df_data, by='BRC_CODE') %>%
          # Drop extra columns
          select(SITE_NAME, CFR, DATE, YEAR, PARAMETER, RESULT, UNITS)

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
        if(nrow(df_param_filter()>0)) {

          if(df_param_filter()$EXC > df_param_filter()$GOOD){
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
        HTML(glue('<h2>{brcvar$catSelect_graph()}</h2>'))
      })

      # * Caption ----

      param_caption <- reactive({

        # Define variables
        sites <- df_site()$SITE_NAME

        site_conductivity <- df_site() %>%
          select(SITE_NAME, CONDUCTIVITY_USCM) %>%
          filter(!is.na(CONDUCTIVITY_USCM))

        site_waterdepth <- df_site() %>%
          select(SITE_NAME, WATER_DEPTH_FT) %>%
          filter(!is.na(WATER_DEPTH_FT))

        param <- brcvar$catSelect_graph()
        units <- df_data_filter()$UNITS[1]

        param_sentence = ''

        # Sentence fragment - list of sites
        if (length(sites) == 3){
          site_list <- glue('{sites[1]}, {sites[2]}, and {sites[3]}')
        } else if (length(sites) == 2) {
          site_list <- glue('{sites[1]} and {sites[2]}')
        } else {
          site_list <- paste(sites[1])
        }

        # Sentence - baseline, min/max values

        # Sentence for conductivity, Water Depth
        if (param %in% c('Conductivity', 'Water Depth')) {
          if(param == 'Conductivity' & nrow(site_conductivity)>0){
            site_names <- site_conductivity$SITE_NAME
            site_values <- site_conductivity$CONDUCTIVITY_USCM
          } else if(param == 'Water Depth' & nrow(site_waterdepth)>0){
            site_names <- site_waterdepth$SITE_NAME
            site_values <- site_waterdepth$WATER_DEPTH_FT
          } else {
            site_names <- 'no data'
            site_values <- -999999
          }

          if (length(site_names) == 3){
            value_list <- glue('{site_values[1]} {units} for {site_names[1]},
                               {site_values[2]} {units} for {site_names[2]}, and
                               {site_values[3]} {units} for {site_names[3]}')
          } else if (length(site_names) == 2) {
            value_list <- glue('{site_values[1]} {units} for {site_names[1]} and
                               {site_values[2]} {units} for {site_names[2]}')
          } else {
            value_list <- glue('{site_values[1]} {units} for {site_names[1]}')
          }

          if (!('no data' %in% site_names)){
            param_sentence <- glue('The baseline {tolower(param)} is
                                 {value_list}.')
          }

          # Sentence for other parameters
        } else if(nrow(df_param_filter())>0){
          param_wwf <- df_param_filter()$AVG
          param_cwf <- df_param_filter()$AVG_CWF

          if(param_wwf == param_cwf){
            param_sentence <- glue('The {param_minmax()} acceptable value is
                                   {param_wwf} {units}.')
          } else {
            param_sentence <- glue('The {param_minmax()} acceptable value is
            {param_wwf} {units} for warmwater fisheries and {param_cwf} {units}
            for coldwater fisheries.')
          }
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
          rename(Site=SITE_NAME, Waterbody=WATERBODY_NAME,
                 'Coldwater Fishery'=CFR, Watershed=HUC12_NAME)  %>%
          # Arrange columns, drop any column not listed
          select(Site, Waterbody, 'Coldwater Fishery', Town, Watershed)

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
        par <- brcvar$catSelect_graph()
        unit <- df_data_filter()$UNITS[1]
        min_par <- min(df_data_filter()$RESULT)
        max_par <- max(df_data_filter()$RESULT)

        df_param_filter <- df_param_filter()

        # Define Y-axis
        ylab <- glue("{par} ({unit})")

        # * Plotlines, plotbands ----

        # Set default null value for plotlines, plotbands
        plotbands_yaxis = NULL
        plotlines_yaxis = NULL

        # Add plotlines, plotbands if data available
        if(nrow(df_param_filter)>0) {
          par_cutoff = df_param_filter$AVG
          par_cutoff_cwf = df_param_filter$AVG_CWF

          # Set lower/upper range for plotbands
          if (df_param_filter$EXC > df_param_filter$GOOD) {
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

        } # End if nrow(df_param_filter)>0

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
