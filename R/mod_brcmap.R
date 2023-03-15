################################### HEADER ###################################
#  TITLE: mod_brcmap.R
#  DESCRIPTION: A module for a simple map for the BRCWQDM App
#  AUTHOR(S): Dan Crocker, Mariel Sorlien
#  DATE LAST UPDATED: 2023-03-15
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(leaflet)
library(leaflegend)

########################################################################.
###                       User Interface                            ####
########################################################################.
BRCMAP_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns("map"), width='100%', height = "85vh")
 ) 
}

########################################################################.
###                       Server Function                           ####
########################################################################.

BRCMAP_SERVER <- function(id, brcvar) {
  moduleServer(
    id, 
    function(input, output, session) {
      # Data ----
      
      # * Site scores ----
      
      # Filter score for selected parameter
      df_score <- reactive({
        req(brcvar$catSelect_1())
        
        df_score <- brcvar$df_score() %>%
          filter(PARAMETER == brcvar$catSelect_1())
        
        return(df_score)
      })
      
      
      # Join site, score
      df_site_score <- reactive({
        
        df_site <- brcvar$df_site()
        df_score <- df_score()

        df_merge <- merge(x=df_site, y=df_score, by='BRC_CODE', all.x=TRUE) %>%
          mutate_at('SCORE', replace_na, 'No Data')
        
        return(df_merge)
      })
      
      # * HUC shapefile ----
      # Filter for selected watersheds
      bs_huc12_filter <- reactive({
        # required inputs
        req(brcvar$riverSelect())
        
        bs_huc12_filter <- shp_huc12 %>%
          filter(HUC12_Name %in% brcvar$riverSelect())
        
        return(bs_huc12_filter)
      })
      
      # * Town shapefile ----
      # Filter for selected towns
      bs_town_filter <- reactive({
        # required inputs
        req(brcvar$townSelect())
        
        bs_town_filter <- shp_town %>%
          filter(Town_Name %in% brcvar$townSelect())
        
        return(bs_town_filter)
      })
      
      # * River shapefile ----
      # Read in data
      bs_river <- shp_river %>%
        mutate(Fishery = case_when(Coldwater=='Yes'~'Coldwater Fishery',
                                   Coldwater=='No'~'Not Coldwater Fishery'))
      
      # Filter for selected watersheds
      bs_river_watershed <- reactive({
        req(brcvar$riverSelect())
        
        df_river <- bs_river %>%
          filter(HUC12_Name %in% brcvar$riverSelect())
        
        return(df_river)
      })
      
      # Filter for selected towns
      bs_river_town <- reactive({
        req(brcvar$townSelect())
        
        df_river <- bs_river %>%
          filter(Town_Name %in% brcvar$townSelect())
        
        return(df_river)
      })
      
      # Toggle between selected towns & watersheds
      bs_river_filter <- reactive({
        if (brcvar$riverTown() == 'river') {
          bs_river_watershed()
        } else {
          bs_river_town()
        }
      })
      
      # Icons ----
      # * Site icons ----
      icon_color <- c('#44BB99', '#BBCC33', '#EEDD88', '#EE8866', '#DDDDDD')
      
      icon_shape <- c('circle', 'rect', 'triangle', 'diamond', 'cross')
      
      icon_names <- c('Excellent', 'Good', 'Fair', 'Poor', 
                     'No Data')
      
      icon_symbols <- setNames(Map(f = makeSymbol,
                                   shape = icon_shape,
                                   fillColor= icon_color, color = 'black',
                                   fillOpacity = 1, opacity = 1, 
                                   height = 24, width = 24,
                                   'stroke-width' = 2), 
                               # Assign name to each symbol
                               nm=icon_names)
      
      # * River icons ----
      river_colors <- c("#08306b", "#4292c6")
      river_names <- c('Yes', 'No')
      
      river_pal <- colorFactor(palette = river_colors,
                               levels = river_names)
      
      river_symbols = setNames(Map(f=makeSymbol,
                                   shape='rect',
                                   fillColor=river_colors, fillOPacity=1,
                                   color=river_colors, opacity=1,
                                   height=10, width=24,
                                   'stroke-width' = 0),
                               nm=river_names)
      
      # Map ----
      output$map <- renderLeaflet({
        leaflet() %>%
          # * Set map dimensions ---- 
          fitBounds(-71.928017, # Lon min
                    41.854794, # Lat min
                    -71.353397, # Lon max
                    42.350757 # Lat max
                    ) %>%
          # * Add basemap tiles ----
          addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>%
          addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
          # * Add legend ----
          addLegendImage(
            images = icon_symbols,
            labels = icon_names,
            width = 20,
            height = 20,
            orientation = 'vertical',
            title = htmltools::tags$div('Parameter Score', 
                                        style = 'font-size: 18px'),
            labelStyle = 'font-size: 18px;',
            position = 'topright',
            # misc
            group = 'Sites'
          ) %>%
          addLegendImage(
            images = river_symbols,
            labels = river_names,
            width = 20,
            height = 4,
            orientation = 'vertical',
            title = htmltools::tags$div('Coldwater Fishery', 
                                        style = 'font-size: 18px'),
            labelStyle = 'font-size: 18px;',
            position = 'topright',
            # Group with river layer
            group = 'Rivers'
          ) %>%
          # * Fancy map options ----
          addScaleBar(position='bottomleft') %>%
          # * Layers control ----
          addLayersControl(
            baseGroups = c('Map', 'Satellite'),
            overlayGroups = c('Sites', 'Rivers', 'Watersheds', 'Towns'),
            position='topleft'
          )
        })
      
      # Add reactive elements to map ----
      observe({ 
        leafletProxy("map") %>%
          clearShapes() %>%
          
          # * HUC boundaries ----
          addPolygons(data = bs_huc12_filter(),
                      layerId = bs_huc12_filter(),
                      # Label
                      label = ~HUC12_Name,
                      labelOptions = labelOptions(textsize = "15px"),
                      # Popup
                      popup = ~paste('<b>Name:</b>', HUC12_Name, 
                                      '<br/><b>ID:</b>', HUC12),
                      # Stroke
                      color = '#08306b',
                      weight = 0.5, 
                      smoothFactor = 0.5,
                      opacity = 0.9, 
                      # Fill
                      fillOpacity = 0.4,
                      fillColor = '#9ecae1',
                      # Highlight
                      highlightOptions = highlightOptions(fillColor = '#f7fbff',
                                                          weight = 2,
                                                          bringToFront = FALSE),
                      # misc
                      group = 'Watersheds'
          ) %>% 
          
          # * Towns ----
          addPolygons(data = bs_town_filter(),
                      layerId = bs_town_filter(),
                      # Label
                      label = ~paste(Town_Name, ", ", State, sep = ""),
                      labelOptions = labelOptions(textsize = "15px"),
                      # Popup
                      popup = ~paste0('<b>', Town_Name, ', ', State, '</b>'),
                      # Stroke
                      color = '#08306b',
                      weight = 0.5, 
                      smoothFactor = 0.5,
                      opacity = 0.9, 
                      # Fill
                      fillOpacity = 0.4,
                      fillColor = '#9ecae1',
                      # Highlight
                      highlightOptions = highlightOptions(fillColor = '#f7fbff',
                                                          weight = 2,
                                                          bringToFront = FALSE),
                      # misc
                      group = 'Towns'
          ) %>%
          # * Rivers ----
          addPolylines(data = bs_river_filter(),
                       layerId = bs_river_filter(),
                       # Label
                       label = ~Fishery,
                       labelOptions = labelOptions(textsize = "15px"),
                       # Popup
                       popup = ~paste0(
                                       '<br/><b>Town: </b>', Town_Name, ', ', 
                                       State, 
                                       '<br/><b>Watershed:</b> ', HUC12_Name, 
                                       '<br/><b>Coldwater Fishery:</b> ', 
                                       Coldwater),
                       # Stroke
                       color = ~river_pal(Coldwater),
                       weight = 2, 
                       smoothFactor = 1,
                       opacity = 1,
                       # Highlight
                       highlightOptions = highlightOptions(color = '#f7fbff',
                                                           weight = 3,
                                                           bringToFront = TRUE),
                       # misc
                       group = 'Rivers')
        
      }) 
      
      # * Toggle HUC, town visibility ---- 
      observe({
        # River vs town
        if (brcvar$riverTown() == 'river') {
          leafletProxy('map') %>% 
            showGroup('Watersheds') %>%
            hideGroup('Towns')
        } else {
          leafletProxy('map') %>%
            showGroup('Towns') %>%
            hideGroup('Watersheds')
        }
      })
      
      # * Sites ----
      observe({ 
        leafletProxy("map") %>%
          clearMarkers() %>%
          
          addMarkers(
            data=df_site_score(),
            lng = ~LONGITUDE, 
            lat = ~LATITUDE,
            # Symbology
            icon = ~icons(
              iconUrl = icon_symbols[SCORE],
              iconWidth = 20,
              iconHeight = 20),
            # Label
            label = ~SITE_NAME,
            labelOptions = labelOptions(textsize = "15px"),
            # Popup
            popup = ~paste0('<b>Site: </b>', SITE_NAME,
                           '<br/><b>Waterbody: </b>', WATERBODY_NAME,
                           '<br/><b>Town: </b>', TOWN, ', ', STATE,
                           '<br/><b>Watershed:</b> ', HUC12_NAME,
                           '<br/><b>Coldwater Fishery:</b> ', CFR,
                           '<br/><br/><b>', brcvar$catSelect_1(), ': </b>', SCORE),
            # Accessibility
            options = markerOptions(
              alt = ~paste(SITE_NAME, SCORE)),
            # misc
            group = 'Sites'
          )
      }) 

  })
} # end Server Function
