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

neg_bin_policy = glm.nb(new_cases ~ month_name + state_name + as.factor(school_closing) 
                        + as.factor(workplace_closing) + as.factor(gatherings_restrictions)
                        + state_tavg, data = covid_noaa_dataset) 

neg_bin_public_health = glm.nb(new_cases ~ month_name + state_name + as.factor(information_campaigns) 
                               + as.factor(testing_policy) + as.factor(contact_tracing)
                               + state_tavg, data = covid_noaa_dataset) 

ui <- fluidPage(
  titlePanel("Case count Predictor"),
  
  tabsetPanel(id='my_tabsetPanel',
    tabPanel('temperature & precipitation',
        sidebarPanel(
          helpText(h2("Enter details")),
          
          selectInput("month1", h3("Select Month"),
                      choices =unique(covid_noaa_dataset["month_name"]),selected = "May"),
          
          selectInput("state1", h3("Select state"),
                      choices =unique(covid_noaa_dataset["state_name"]),selected = "New York"),
          
          sliderInput( "tavg1",
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
           #fluidRow(
            # column(15, leafletOutput("selected_var"))
           #)
          leafletOutput("plot1"),
          h3("Prediction of the number of new cases for a particular month based on average temperature and 
             precipitation"),
          p("Based on the user's choice of temperature and precipitation, the negative binomial regression 
          model will predict the number of new cases that are expected for a particular month and
            state in the United States")
        )
      
    ),
    tabPanel('Restrictions',
             sidebarPanel(
               helpText(h2("Enter details")),
               
               selectInput("month3", h3("Select Month"),
                           choices =unique(covid_noaa_dataset["month_name"]),selected = "May"),
               
               selectInput("state3", h3("Select State"),
                           choices =unique(covid_noaa_dataset["state_name"]),selected = "New York"),
               
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
                                          "Limit upto 10 people" = 4),
                           selected = 0),
               
               sliderInput( "tavg3",
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
               leafletOutput("plot3"),
               h3("Prediction of the number of new cases for a particular month based on restrictions"),
               p("Based on the user's choice of different restrictions and temperature , the negative binomial regression 
          model will predict the number of new cases that are expected for a particular month and
            state in the United States")
             )
      ),
    tabPanel('Public Health Policies',
             sidebarPanel(
               helpText(h2("Enter details")),
               
               selectInput("month4", h3("Select Month"),
                           choices =unique(covid_noaa_dataset["month_name"]),selected = "May"),
               
               selectInput("state4", h3("Select State"),
                           choices =unique(covid_noaa_dataset["state_name"]),selected = "New York"),
               
               selectInput("campaign", h3("Select information campaign Policy"),
                           choices = list("No covid 19 public campaign" = 0, "Public officials urging caution " = 1,
                                          "Coordinated public information campaign" = 2),
                           selected = 1),
               
               selectInput("testing", h3("Select testing Policy"),
                           choices = list("No testing policy" = 0, "those with symptoms and  specific criteria (intl travel) tested" = 1,
                                          "Testing anyone with symptoms" = 2, "Open public testing" = 3),
                           selected = 2),
               
               selectInput("contact", h3("Select contatc tracing Policy"),
                           choices = list("No contact tracing" = 0, "Limited contact tracing" = 1,
                                          "Comprehensive contact tracing" = 2),
                           selected = 2),
               
               sliderInput( "tavg4",
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
               leafletOutput("plot4"),
               h3("Prediction of the number of new cases for a particular month based on health sector activity"),
               p("Based on the user's choice of various health sector policies, the negative binomial regression 
          model will predict the number of new cases that are expected for a particular month and
            state in the United States")
             )
    ),
    tabPanel('General Trend',
             sidebarPanel(
               helpText(h2("Enter details")),
               
               selectInput("month2", h3("Select Month"),
                           choices =unique(covid_noaa_dataset["month_name"]),selected = "August")
             ),
             
             mainPanel(
               # fluidRow(
               #   column(12,label = "Prediction", leafletOutput("selected_var")),
               #   column(12, label = "general trend", leafletOutput("plot1"))
               # )
               leafletOutput("plot2"),
               h3("General trend of the number of new cases for a particular month"),
               p("This gives the general trend of the new covide cases thats actually been seen. the user can select the month
                 to obeserve the covid case change over time")
             )
             
    )
  )
)


server <- function(input, output) {
  
  output$plot1 <- renderLeaflet({
    
    df = data.frame( month_name = input$month1, state_name = input$state1 ,
                     state_tavg = input$tavg1 , state_total_prcp = input$prcp )
    
    count = as.integer( predict(neg_bin_mod, df, type = "response") )
    
    display = sprintf(
      "<strong>Predicted number of cases: %s</strong>",
      count
    ) %>% lapply(htmltools::HTML)
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
      filter(month_name == as.factor(input$month1),
             state_name ==input$state1)
    
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("state" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "state")
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_df$state_tavg)
    
    leaflet(states_merged_sb)%>%
      setView(-96, 40, 3.5) %>%
      addPolygons(
        fillColor = ~pal(states_merged_sb$state_tavg),
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
    map_df = covid_noaa_dataset%>% filter(month_name == input$month2)
    
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
          bringToFront = TRUE),
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
    
    
    df = data.frame( month_name = input$month3, state_name = input$state3, school_closing = as.factor(input$school),
                     workplace_closing = as.factor(input$workplace), gatherings_restrictions = as.factor(input$gathering),
                     state_tavg = input$tavg3 )
    
    count = as.integer( predict(neg_bin_policy, df, type = "response") )
    
    display = sprintf(
      "<strong>Predicted number of cases: %s</strong>",
      count
    ) %>% lapply(htmltools::HTML)
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
      filter(month_name == input$month3,
             state_name ==input$state3)
    
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("state" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "state")
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_df$state_tavg)
    
    leaflet(states_merged_sb)%>%
      setView(-96, 40, 3.5) %>%
      addPolygons(
        
        fillColor = ~pal(states_merged_sb$state_tavg),
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
  
  output$plot4 <- renderLeaflet({
    
    
    df = data.frame( month_name = input$month4, state_name = input$state4, information_campaigns = as.factor(input$campaign),
                     testing_policy = as.factor(input$testing), contact_tracing = as.factor(input$contact),
                     state_tavg = input$tavg4 )
    

    
    count = as.integer( predict(neg_bin_public_health, df, type = "response") )
    
    display = sprintf(
      "<strong>Predicted number of cases: %s</strong>",
      count
    ) %>% lapply(htmltools::HTML)
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
      filter(month_name == input$month4,
             state_name == input$state4)
    
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("state" = "state" ))
    
    states_merged_sb <- geo_join(states, map_df, "STUSPS", "state")
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_df$state_tavg)
    
    leaflet(states_merged_sb)%>%
      setView(-96, 40, 3.5) %>%
      addPolygons(
        fillColor = ~pal(states_merged_sb$state_tavg),
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