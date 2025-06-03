library(sf)
library(shiny)
library(shinyWidgets)
library(tidyverse)

df_default <- data.table::fread("findings-df.csv")

ui <- fluidPage(
  titlePanel("The Distribution of the UCLA Herbarium's Botanical Specimen Findings"),
  sidebarLayout(
    sidebarPanel(radioButtons("df_source", "Choose Data Source:",
                              choices = c("Use default dataset", "Upload CSV (Darwin Core Format)"),
                              selected = "Use default dataset"),
                 conditionalPanel(
                   condition = "input.df_source == 'Upload CSV (Darwin Core Format)'",
                   fileInput("file_df", "Upload File", accept = ".csv")),
                 fluidRow(downloadButton("downloadData", "Download default CSV")),
                 div(style = "height:10px"),
                 fluidRow(actionButton("show_fam", "Include all families?"),
                          actionButton("clear_fam", "Clear all families?")),
                 fluidRow(actionButton("show_ppl", "Include all collectors?"),
                          actionButton("clear_ppl", "Clear all collectors?")),
                 checkboxGroupInput("season", "Season of discovery", 
                                    choices = unique(herb_df$season),
                                    selected = unique(herb_df$season)),
                 sliderInput("year_range", "Year range", min = 1800, max = 2030,
                             value = c(1900, 2000), sep = ""),
                 sliderInput("elevation_range", "Elevation range", 
                             min = -100, max = 6900, value = c(-100, 1900), sep = ""),
                 pickerInput("family", "Family", choices = sort(unique(herb_df$family)),
                             selected = sort(unique(herb_df$family)), multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE)),
                 pickerInput("surname", "Last name of collector", choices = sort(unique(herb_df$surname)),
                             selected = sort(unique(herb_df$surname)), multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE)),
                 h5("Click on a point to show its species name!"),
                 verbatimTextOutput("info")
    ),
    mainPanel(h4("To zoom in, click and drag cursor to create box, then double-click"),
              h4("To zoom out, double-click map"),
              tabsetPanel(
                id = "plots",
                tabPanel("Main plot", 
                         plotOutput("map_plot_main",
                                    dblclick = "plot1_dblclick",
                                    brush = brushOpts(id = "plot1_brush",
                                                      resetOnNew = TRUE),
                                    click = "plot_click")),
                tabPanel("Contour plot",
                         plotOutput("map_plot_contour",
                                    dblclick = "plot1_dblclick",
                                    brush = brushOpts(id = "plot1_brush",
                                                      resetOnNew = TRUE),
                                    click = "plot_click"))
              )
    )
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  input_df <- reactive({
    if (input$df_source == "Use default dataset") {
      return(df_default)
    }
    else {
      req(input$file_df)
      return(data.table::fread(input$file_df$datapath))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "herbarium_data.csv",
    content = function(file) {
      write.csv(df_default, file)
    }
  )
  
  herb_df <- reactive({
    input_df() %>%
      select(family, genus, specificEpithet, scientificName, recordedBy, 
             minimumElevationInMeters, year, month, habitat,
             decimalLatitude, decimalLongitude, recordEnteredBy) %>%
      rename(elevation = minimumElevationInMeters,
             species = specificEpithet,
             latitude = decimalLatitude,
             longitude = decimalLongitude,
             entered_by = recordEnteredBy) %>%
      mutate(season = case_when(month %in% c(1:2, 12) ~ "Winter (December, January, February)",
                                month %in% 3:5 ~ "Spring (March, April, May)",
                                month %in% 6:8 ~ "Summer (June, July, August)",
                                month %in% 9:11 ~ "Fall (September, October, November)"),
             surname = str_extract(recordedBy, "[:alpha:]+$")) %>%
      filter(!is.na(season), !is.na(longitude), !is.na(latitude), 
             !is.na(entered_by), str_detect(family, "[:alpha:]")) %>%
      filter(!(family %in% c("CMP", "LAB")))
  })
  
  world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE, ylim = c(-180, 20)), crs = 4326)
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
  counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
  CA_counties <- subset(counties, grepl("california", counties$ID))
  
  observeEvent(input$show_fam, {
    updatePickerInput(session, "family", selected = unique(sort(herb_df()$family)))
  })
  observeEvent(input$clear_fam, {
    updatePickerInput(session, "family", selected = character(0))
  })
  
  observeEvent(input$show_ppl, {
    updatePickerInput(session, "surname", selected = unique(sort(herb_df()$surname)))
  })
  observeEvent(input$clear_ppl, {
    updatePickerInput(session, "surname", selected = character(0))
  })
  
  map_ranges <- reactiveValues(x = c(-125, -113.5), y = c(30, 42.5))
  
  new_herb_df <- reactive({herb_df()[herb_df()$season %in% input$season &
                                       herb_df()$year %in% input$year_range[1]:input$year_range[2] &
                                       herb_df()$elevation %in% input$elevation_range[1]:input$elevation_range[2] &
                                       herb_df()$surname %in% input$surname &
                                       herb_df()$family %in% input$family, ]})
  
  herb_sf <- reactive({st_as_sf(herb_df()[herb_df()$season %in% input$season &
                                            herb_df()$year %in% input$year_range[1]:input$year_range[2] &
                                            herb_df()$elevation %in% input$elevation_range[1]:input$elevation_range[2] &
                                            herb_df()$surname %in% input$surname &
                                            herb_df()$family %in% input$family, ],
                                coords = c("longitude", "latitude"), crs = 4326)})
  
  output$map_plot_main <- renderPlot({
    ggplot() +
      geom_sf(data = world) +
      geom_sf(data = states) +
      geom_sf(data = CA_counties, fill = NA) +
      geom_sf(data = herb_sf(), alpha = 0.5) +
      coord_sf(xlim = map_ranges$x, map_ranges$y, expand = FALSE)
  }, width = 800, height = 800)
  
  output$map_plot_contour <- renderPlot({
    ggplot() +
      geom_sf(data = world) +
      geom_sf(data = states) +
      geom_sf(data = CA_counties, fill = NA) +
      geom_sf(data = herb_sf(), alpha = 0.5) +
      coord_sf(xlim = map_ranges$x, map_ranges$y, expand = FALSE) +
      stat_density_2d_filled(data = new_herb_df(), 
                             alpha = 0.5, 
                             n = 250,
                             contour_var = "density",
                             aes(x = longitude, y = latitude, 
                                 fill = as.numeric(..level..))) +
      stat_density_2d(data = new_herb_df(), 
                      alpha = 0.75, 
                      n = 250,
                      contour = TRUE,
                      contour_var = "density",
                      aes(x = longitude, y = latitude)) +
      scale_fill_gradient2("Number of occurences per unit area", 
                           low = "white", 
                           mid = "green", 
                           high = "darkgreen", 
                           limits = c(0, 8),
                           midpoint = 4)
  }, width = 800, height = 800)
  
  output$info <- renderPrint({
    if (is.null(input$plot_click)) return(noquote(""))
    paste0("Species name: ", 
           nearPoints(new_herb_df(), input$plot_click, xvar = "longitude", yvar = "latitude")$scientificName)
  })
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      map_ranges$x <- c(brush$xmin, brush$xmax)
      map_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      map_ranges$x <- c(-125, -113.5)
      map_ranges$y <- c(30, 42.5)
    }
  })
}

shinyApp(ui, server)
