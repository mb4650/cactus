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
library(sp)
library(tigris)
library(MASS)


covid_noaa_dataset = read_csv("covid_noaa_dataset.csv")
states <- states(cb=T)



# Downloading the shapefiles for states at the lowest resolution

url = "https://urldefense.proofpoint.com/v2/url?u=https-3A__developers.google.com_public-2Ddata_docs_canonical_states-5Fcsv&d=DwIFAg&c=G2MiLlal7SXE3PeSnG8W6_JBU6FcdVjSsBSbw6gcR0U&r=B8uzIkNMhKdWydN9xY4NUSbhsqKRbTFG_gmZY3kin8Q&m=ZLDhVDaJRa8xTJd2UCndV5ZKTHV5ZrzOcRkhqHloTko&s=RJ-z_AUb_Xy-Yw9rP8euzOmNJCXWGMMWkgyuhy97A8M&e= "
lat_long_html = read_html(url)

table_lat_long_df =
  lat_long_html %>%
  html_nodes(css = "table") %>%
  first() %>%
  html_table()

neg_bin_mod = glm.nb(new_cases ~ month_name + state_name + state_tavg + state_total_prcp,
                     data = covid_noaa_dataset)

neg_bin_policy = glm.nb(new_cases ~ as.factor(month) + state_name + school_closing + workplace_closing + gatherings_restrictions, 
                        
                        data = covid_noaa_dataset) 

ui <- fluidPage(
  titlePanel("Case count Predictor"),
  
  tabsetPanel(id='my_tabsetPanel',
    tabPanel('Map1',
        sidebarPanel(
          helpText(h2("Enter details")),
          
          selectInput("Month1", h3("Select Month"),
                      choices =unique(covid_noaa_dataset["month_name"]),selected = "May"),
          
          selectInput("State", h3("Select state"),
                      choices =unique(covid_noaa_dataset["state_name"]),selected = "New York"),
          
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
          # fluidRow(
          #   column(12,label = "Prediction", leafletOutput("selected_var")),
          #   column(12, label = "general trend", leafletOutput("plot1"))
          # )
          leafletOutput("plot1")
        )
      
    ),
    tabPanel('Map2',
             sidebarPanel(
               helpText(h2("Enter details")),
               
               selectInput("Month2", h3("Select Month"),
                           choices =unique(covid_noaa_dataset["month"]),selected = 4)
             ),
             
             mainPanel(
               # fluidRow(
               #   column(12,label = "Prediction", leafletOutput("selected_var")),
               #   column(12, label = "general trend", leafletOutput("plot1"))
               # )
               leafletOutput("plot2")
             )
             
    ),
    tabPanel('Map3',
             sidebarPanel(
               helpText(h2("Enter details")),
               
               selectInput("month3", h3("Select Month"),
                           choices =unique(covid_noaa_dataset["month"]),selected = 4),
               
               selectInput("state1", h3("Select State"),
                           choices =unique(covid_noaa_dataset["state_name"]),selected = 10),
               
               selectInput("school", h3("Select School Policy"),
                           choices = list("No measures" = 0, "Recommended closing" = 1,
                                          "Require closing some schools" = 2, "Require closing all schools" = 3),
                          selected = 0),
               
               selectInput("workplace", h3("Select Workplace Policy"),
                           choices = list("No measures" = 0, "Recommended work from home" = 1,
                                          "Require closing some sectors" = 2, "Only essential sectors open" = 3),
                           selected = 0),
               
               selectInput("gathering", h3("Select Gathering Policy"),
                           choices = list("No restrictions" = 0, "Limit very large gatherings" = 1,
                                          "Limit upto 100-1000 people " = 2, "Limit upto 10-100 people" = 3, 
                                          "Limit upto 10 people"),
                           selected = 0),
               
               sliderInput( "tavg1",
                            label = "Select Average temperature",
                            min = 1,
                            max = max(covid_noaa_dataset["state_tmax"])+20,
                            value = 30),
               
             ),
             
             mainPanel(
               # fluidRow(
               #   column(12,label = "Prediction", leafletOutput("selected_var")),
               #   column(12, label = "general trend", leafletOutput("plot1"))
               # )
               leafletOutput("plot3")
             )
      )
  )
)


server <- function(input, output) {
  
  output$plot1 <- renderLeaflet({
    
    df = data.frame( month_name = as.factor(input$Month1), state_name = input$State ,
                     state_tavg = input$tavg , state_total_prcp = input$prcp )
    
    count = as.integer( predict(neg_bin_mod, df, type = "response") )
    
    display = sprintf(
      "<strong>Predicted number of cases: %s</strong>",
      count
    ) %>% lapply(htmltools::HTML)
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
      filter(month == as.factor(input$Month1),
             state_name ==input$State)
    
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("state" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "state")
    
    
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
                       radius =count/40000,color = "red",popup =map_df$state_name) %>%
      
      addControl(display, position = "bottomleft")
    
  })
  
  output$plot2 <- renderLeaflet({
    map_df = covid_noaa_dataset%>% filter(month == as.factor(input$Month2))
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("state" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "state")
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_df$state_tavg)
    
    
    pal1 <- colorNumeric(
      palette = colorRampPalette(c('red', 'dark red'))(length(map_df$new_cases)), 
      domain = map_df$new_cases)
    
    
   
    
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
  
  output$plot3 <- renderLeaflet({
    
    df = data.frame( month = as.factor(input$Month1), state_name = input$State ,
                     state_tavg = input$tavg , state_total_prcp = input$prcp )
    
    count = as.integer( predict(neg_bin_mod, df, type = "response") )
    
    display = sprintf(
      "<strong>Predicted number of cases: %s</strong>",
      count
    ) %>% lapply(htmltools::HTML)
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
      filter(month == as.factor(input$Month1),
             state_name ==input$State)
    
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("state" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "state")
    
    
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
                       radius =count/40000,color = "red",popup =map_df$state_name) %>%
      
      addControl(display, position = "bottomleft")
    
  })
  
}

shinyApp(ui,server)