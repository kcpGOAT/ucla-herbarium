library(sf)
library(shiny)
library(shinyWidgets)
library(tidyverse)

herbarium_data <- data.table::fread("findings-df.csv")

herb_df <- herbarium_data %>%
  select(family, genus, specificEpithet, recordedBy, 
         minimumElevationInMeters, year, month, habitat,
         decimalLatitude, decimalLongitude, recordEnteredBy) %>%
  rename(elevation = minimumElevationInMeters,
         species = specificEpithet,
         latitude = decimalLatitude,
         longitude = decimalLongitude,
         entered_by = recordEnteredBy) %>%
  mutate(season = case_when(month %in% c(1:2, 12) ~ "winter",
                            month %in% 3:5 ~ "spring",
                            month %in% 6:8 ~ "summer",
                            month %in% 9:11 ~ "fall")) %>%
  filter(!is.na(season), !is.na(longitude), !is.na(latitude), !is.na(entered_by))

ui <- fluidPage(
  titlePanel("The Distribution of UCLA's Botanical Specimen Findings"),
  sidebarLayout(
    sidebarPanel(actionButton("show_fam", "Include all families?"),
                 actionButton("clear_fam", "Clear all families?"),
                 checkboxGroupInput("season", "Season of discovery", 
                                    choices = unique(herb_df$season),
                                    selected = unique(herb_df$season)),
                 sliderInput("year_range", "Year range", min = 1800, max = 2030,
                             value = c(1900, 2000), sep = ""),
                 sliderInput("elevation_range", "Elevation range", 
                             min = -100, max = 7500, value = c(100, 2000), sep = ""),
                 pickerInput("family", "Family", choices = sort(unique(herb_df$family)),
                             selected = sort(unique(herb_df$family)), multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE))
    ),
    mainPanel(h4("To zoom in, click and drag cursor to create box, then double-click"),
              h4("To zoom out, double-click map"),
              plotOutput("map_plot",
                         dblclick = "plot1_dblclick",
                         brush = brushOpts(id = "plot1_brush",
                                           resetOnNew = TRUE)))
  )
)

server <- function(input, output, session) {
  world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE), crs = 4326)
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
  counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
  CA_counties <- subset(counties, grepl("california", counties$ID))
  
  observeEvent(input$show_fam, {
    updatePickerInput(session, "family", selected = unique(sort(herb_df$family)))
  })
  observeEvent(input$clear_fam, {
    updatePickerInput(session, "family", selected = character(0))
  })
  
  map_ranges <- reactiveValues(x = c(-125, -112), y = c(30, 45))
  
  herb_sf <- reactive({st_as_sf(herb_df[herb_df$season %in% input$season &
                                          herb_df$year %in% input$year_range[1]:input$year_range[2] &
                                          herb_df$elevation %in% input$elevation_range[1]:input$elevation_range[2] &
                                          herb_df$family %in% input$family, ], 
                                coords = c("longitude", "latitude"), crs = 4326)})
  
  output$map_plot <- renderPlot({
    ggplot() +
      geom_sf(data = world) +
      geom_sf(data = states) +
      geom_sf(data = CA_counties, fill = NA) +
      geom_sf(data = herb_sf(), alpha = 0.5) +
      coord_sf(xlim = map_ranges$x, map_ranges$y, expand = FALSE)
  }, width = 800, height = 800)
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      map_ranges$x <- c(brush$xmin, brush$xmax)
      map_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      map_ranges$x <- c(-125, -112)
      map_ranges$y <- c(30, 45)
    }
  })
}

shinyApp(ui, server)
