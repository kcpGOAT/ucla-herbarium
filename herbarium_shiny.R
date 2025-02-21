library(sf)
library(shiny)
library(shinyWidgets)
library(tidyverse)

herb_df <- herbarium_data %>%
  select(family, genus, specificEpithet, recordedBy, 
         minimumElevationInMeters, year, month, habitat,
         decimalLatitude, decimalLongitude) %>%
  rename(elevation = minimumElevationInMeters,
         species = specificEpithet,
         latitude = decimalLatitude,
         longitude = decimalLongitude) %>%
  mutate(season = case_when(month %in% c(1:2, 12) ~ "winter",
                            month %in% 3:5 ~ "spring",
                            month %in% 6:8 ~ "summer",
                            month %in% 9:11 ~ "fall")) %>%
  filter(!is.na(season), !is.na(longitude), !is.na(latitude))

ui <- fluidPage(
  titlePanel("The Distribution of UCLA's Botanical Specimen Findings"),
  sidebarLayout(
    sidebarPanel(checkboxInput("all_spec", "Show all specimens?"),
                 checkboxGroupInput("season", "Season of discovery", 
                                    choices = unique(herb_df$season)),
                 sliderInput("year_range", "Year range", min = 1800, max = 2030,
                             value = c(1900, 2000), sep = ""),
                 sliderInput("elevation_range", "Elevation range", 
                             min = -100, max = 7500, value = c(100, 2000), sep = ""),
                 pickerInput("family", "Family", choices = sort(unique(herb_df$family)),
                             multiple = TRUE, options = pickerOptions(liveSearch = TRUE)),
    ),
    mainPanel(plotOutput("map_plot"))
  )
)

server <- function(input, output, session) {
  world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE), crs = 4326)
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
  herb_sf <- reactive({st_as_sf(herb_df[herb_df$season %in% input$season &
                                          herb_df$year %in% input$year_range[1]:input$year_range[2] &
                                          herb_df$elevation %in% input$elevation_range[1]:input$elevation_range[2] &
                                          herb_df$family %in% input$family, ], 
                                coords = c("longitude", "latitude"), crs = 4326)})
  
  output$map_plot <- renderPlot({
    if (input$all_spec == TRUE) {
      ggplot() + 
        geom_sf(data = world) +
        geom_sf(data = states) +
        geom_sf(data = st_as_sf(herb_df, coords = c("longitude", "latitude"), crs = 4326), alpha = 0.5) +
        coord_sf(xlim = c(-125, -112), ylim = c(30, 45), expand = FALSE)
    }
    else {
      ggplot() +
        geom_sf(data = world) +
        geom_sf(data = states) +
        geom_sf(data = herb_sf(), alpha = 0.5) +
        coord_sf(xlim = c(-125, -112), ylim = c(30, 45), expand = FALSE)
    } 
  }, width = 800, height = 800)
}

shinyApp(ui, server)
