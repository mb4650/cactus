library(shiny)
library(readr)
library(leaflet)
library(maps)
library(tidyverse)
library(COVID19)
library(lubridate)
library(modelr)
library(mgcv)
library(rvest)
library(httr)

covid_noaa_dataset = read_csv("covid_noaa_dataset.csv")
states <- states(cb=T)

url = "https://urldefense.proofpoint.com/v2/url?u=https-3A__developers.google.com_public-2Ddata_docs_canonical_states-5Fcsv&d=DwIFAg&c=G2MiLlal7SXE3PeSnG8W6_JBU6FcdVjSsBSbw6gcR0U&r=B8uzIkNMhKdWydN9xY4NUSbhsqKRbTFG_gmZY3kin8Q&m=ZLDhVDaJRa8xTJd2UCndV5ZKTHV5ZrzOcRkhqHloTko&s=RJ-z_AUb_Xy-Yw9rP8euzOmNJCXWGMMWkgyuhy97A8M&e= "
lat_long_html = read_html(url)

table_lat_long_df =
  lat_long_html %>%
  html_nodes(css = "table") %>%
  first() %>%
  html_table()

neg_bin_mod = glm.nb(new_cases ~ as.factor(month) + state_name + state_tavg + state_total_prcp,
                     data = covid_noaa_dataset)

ui <- fluidPage(
  titlePanel("Case count Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(h2("enter details")),
      
      selectInput("Month", h3("Select Month"),
                  choices =unique(covid_noaa_dataset["month"]),selected = 4),
      
      selectInput("State", h3("Select state"),
                  choices =unique(covid_noaa_dataset["state_name"]),selected = 10),
      
      sliderInput( "tavg",
                   label = "Select Average temperature",
                   min = 1,
                   max = max(covid_noaa_dataset["state_tmax"])+20,
                   value = 30),
      
      sliderInput(inputId = "prcp",
                  label = "Select precipitation",
                  min = -10,
                  max = max(covid_noaa_dataset["state_total_prcp"])+10,
                  value = 1)
      
    ),
    
    mainPanel(
      fluidRow(
        column(12,label = "Prediction", leafletOutput("selected_var")),
        column(12, label = "general trend", leafletOutput("plot1"))
      )
    )
  )
)


server <- function(input, output) {
  
  output$selected_var <- renderLeaflet({
    
    df = data.frame( month = as.factor(input$Month), state_name = input$State ,
                     state_tavg = input$tavg , state_total_prcp = input$prcp )
    
    count = as.integer( predict(neg_bin_mod, df, type = "response") )
    
    display = sprintf(
      "<strong>Predicted number of cases: %s</strong>",
      count
    ) %>% lapply(htmltools::HTML)
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
      filter(month == as.factor(input$Month),
             state_name ==input$State)
    
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("key_alpha_2" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "key_alpha_2")
    
    
    leaflet(states_merged_sb)%>%
      setView(-96, 40, 3.5) %>%
      addPolygons(
        fillColor = "pink",
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = FALSE)) %>%
      
      addCircleMarkers(lat =map_df$latitude.y, lng = map_df$longitude.y,
                       radius =count/20000,color = "red",popup =map_df$state_name) %>%
      
      addControl(display, position = "bottomleft")
    
  })
  
  output$plot1 <- renderLeaflet({
    map_df = covid_noaa_dataset%>% filter(month == as.factor(input$Month))
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("key_alpha_2" = "state" ))
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_df$state_tavg)
    
    
    pal1 <- colorNumeric(
      palette = colorRampPalette(c('red', 'dark red'))(length(map_df$new_cases)), 
      domain = map_df$new_cases)
    
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "key_alpha_2")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      states_merged_sb$state_name, states_merged_sb$new_cases
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states_merged_sb)%>%addTiles()%>%
      setView(-97, 40, 4) %>%
      addPolygons(
        fillColor = ~pal(states_merged_sb$state_tavg),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = FALSE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addCircleMarkers(lat =states_merged_sb$latitude.y,
                       lng = states_merged_sb$longitude.y,
                       radius = states_merged_sb$new_cases/20000,
                       color = pal1(states_merged_sb$new_cases), 
                       popup =states_merged_sb$confirmed ) %>%
      
      addLegend("bottomright", pal = pal, values = ~states_merged_sb$state_tavg,
                title = "temp",
                labFormat = labelFormat(prefix = "F"),
                opacity = 1)
  })
  
}

shinyApp(ui,server)