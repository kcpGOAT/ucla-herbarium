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

hab_pat <- c("hill", "ravine|creek|river", "rock", "wood", "slope")
hab_type <- c("hilly", "aquatic", "rocky", "wooded", "sloped")
herb_df$habitat_type <- ""

for (i in seq_len(5)) {
  herb_df$habitat_type <- ifelse(grepl(hab_pat[i], herb_df$habitat), 
                                 paste(herb_df$habitat_type, hab_type[i]),
                                 herb_df$habitat_type) 
}

herb_df$habitat_type <- ifelse(herb_df$habitat_type == "", "NA", herb_df$habitat_type)
  
ui <- fluidPage(
  titlePanel("The Distribution of UCLA's Botanical Specimen Findings"),
  sidebarLayout(
    sidebarPanel(checkboxInput("all_spec", "Show all specimens?"),
                 checkboxGroupInput("season", "Season of discovery", 
                               choices = unique(herb_df$season)),
                 checkboxGroupInput("habitat", "Habitat of the nearby area",
                                    choices = c("hilly", "aquatic", "rocky", 
                                                "wooded", "sloped", "NA")),
                 sliderInput("year_range", "Years selected", min = 1800, max = 2030,
                             value = c(1900, 2000), sep = ""),
                 sliderInput("elevation_range", "Range of elevations selected", 
                             min = -100, max = 7500, value = c(100, 2000), sep = ""),
                 pickerInput("family", "Family", choices = sort(unique(herb_df$family)),
                             options = pickerOptions(liveSearch = TRUE)),
                 pickerInput("genus", "Genus", choices = sort(unique(herb_df$genus))),
                 textInput("species", "Species", value = "")),
    mainPanel(plotOutput("map_plot"))
  )
)

server <- function(input, output, session) {
    observe({
      new_genus <- herb_df$genus[herb_df$family %in% input$family]
      updatePickerInput(session = session, inputId = "genus", 
                        choices = sort(unique(new_genus)))
    })
    
    world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE), crs = 4326)
    states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
    herb_sf <- reactive({st_as_sf(herb_df[herb_df$season %in% input$season &
                                          grepl(paste(input$habitat, collapse = "|"), herb_df$habitat_type) &
                                          herb_df$year %in% input$year_range[1]:input$year_range[2] &
                                          herb_df$elevation %in% input$elevation_range[1]:input$elevation_range[2] &
                                          herb_df$family %in% input$family &
                                          herb_df$genus %in% input$genus, ], 
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
    })
}

shinyApp(ui, server)
