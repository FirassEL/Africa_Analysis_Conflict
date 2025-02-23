# Required Libraries 
library(shiny)
library(rsconnect)
library(tidyverse)
library(paletteer)
library(sf)
library(janitor)
library(viridis)
library(plotly)
library(stringr)
library(gganimate)
library(shinythemes)
library(leaflet)
library(leaflet.providers)
library(lubridate)

# Loading Data 
Africa_DATA0 <-  read_csv("Africa_1997-2025_Feb07.csv") %>%   
  clean_names()

comm_area <- st_read("Africa_Countries.shp") %>%   
  clean_names()

# Merging dataset with coordinates 
Africa_DATA0 <- Africa_DATA0 %>%   
  filter(!is.na(longitude) & !is.na(latitude) & 
           longitude != 0 & latitude != 0) %>%   
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = st_crs(comm_area))

# Spatial Join 
joined_sf <- st_join(Africa_DATA0, comm_area, join = st_within) %>%   
  mutate(year = as.integer(year))

# Processing dataset with fatalities
processed_data <- joined_sf %>%   
  tibble() %>%   
  group_by(country, event_type, disorder_type, sub_event_type, year) %>%   
  summarise(count_events = n(), total_fatalities = sum(fatalities, na.rm = TRUE), .groups = 'drop') %>%   
  mutate(country = str_trim(country)) %>%   
  group_by(country, year) %>%   
  slice_max(order_by = count_events, n = 1) %>%   
  ungroup() %>%   
  group_by(country, year) %>%   
  mutate(
    most_frequent_event = event_type,
    most_frequent_disorder = disorder_type,
    most_frequent_sub_event = sub_event_type
  ) %>%   
  ungroup() %>%   
  left_join(comm_area, by = c("country" = "name")) %>%   
  st_as_sf()

# Ensure projection
processed_data <- st_transform(processed_data, crs = 4326)
comm_area <- st_transform(comm_area, crs = 4326)

# UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Most Frequent Types of Armed Conflicts by Country in Africa"),
  sidebarPanel(
    selectInput("category", "Select Categories:",
                choices = c("Event Type" = "event_type", 
                            "Disorder Type" = "disorder_type", 
                            "Sub Event Type" = "sub_event_type"),
                selected = "event_type"),
    uiOutput("sub_category_ui"),
    div(style = "width: 100%; padding-bottom: 10px;",
        sliderInput("year", "Select Year:", 
                    min = min(processed_data$year, na.rm = TRUE), 
                    max = max(processed_data$year, na.rm = TRUE), 
                    value = min(processed_data$year, na.rm = TRUE),
                    step = 1, 
                    animate = TRUE))),
  mainPanel(
    leafletOutput("map_plot", height = "700px")
  )
)

# Server logic
server <- function(input, output, session) {
  observeEvent(input$category, {
    req(input$category)
    choices <- switch(input$category,
                      "event_type" = unique(processed_data$event_type),
                      "disorder_type" = unique(processed_data$disorder_type),
                      "sub_event_type" = unique(processed_data$sub_event_type))
    output$sub_category_ui <- renderUI({
      selectizeInput("sub_category", "Select Sub-Categories:",
                     choices = choices,
                     selected = choices[1],
                     multiple = TRUE,
                     options = list(maxItems = 6, placeholder = "Select up to 6 categories"))
    })
  })
  
  filtered_data <- reactive({
    req(input$category)
    if (is.null(input$sub_category) || length(input$sub_category) == 0) {
      return(NULL)
    }
    processed_data %>%   
      filter(year == input$year, get(input$category) %in% input$sub_category) %>%   
      group_by(country, year) %>%   
      mutate(most_frequent = case_when(
        input$category == "event_type" ~ event_type,
        input$category == "disorder_type" ~ disorder_type,
        input$category == "sub_event_type" ~ sub_event_type,
        TRUE ~ event_type
      )) %>%   
      ungroup()
  })
  
  output$map_plot <- renderLeaflet({
    leaflet() %>%   
      addTiles() %>%   
      setView(lng = 20, lat = 0, zoom = 3)
  })
  
  observe({
    data <- filtered_data()
    leafletProxy("map_plot") %>%   
      clearShapes() %>%   
      clearControls()
    if (!is.null(data) && nrow(data) > 0) {
      pal <- colorFactor("magma", data$most_frequent)
      leafletProxy("map_plot", data = data) %>%   
        addPolygons(data = comm_area,   
                    color = "black",   
                    weight = 1,   
                    fillColor = "white",   
                    fillOpacity = 0.3) %>%   
        addPolygons(data = data,   
                    fillColor = ~pal(most_frequent),   
                    color = "black",   
                    weight = 1,   
                    fillOpacity = 0.7,   
                    label = ~paste0("Click for more info"),   
                    popup = ~paste("<b>Country:</b>", country, "<br>",
                                   "<b>Event:</b>", most_frequent, "<br>",
                                   "<b>Year:</b>", year, "<br>",
                                   "<b>Count:</b>", count_events, "<br>",
                                   "<b>Fatalities:</b>", total_fatalities),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%   
        addLegend("bottomright",   
                  pal = pal,   
                  values = data$most_frequent,   
                  title = "Most Frequent Event",   
                  opacity = 1) %>% addControl(
                    html = "<div style='font-size:12px; padding: 5px;'>Data source: ACLED (Accessed February 2025) | <a href='https://acleddata.com'>acleddata.com</a></div>",
                    position = "bottomright"
                  )
    }
  })
}

shinyApp(ui = ui, server = server)
