library(shiny)
library(readr)

covid_noaa_dataset = read_csv("data/covid_noaa_dataset.csv")

ui <- fluidPage(
  titlePanel("map_df Case count Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(""),
      
      selectInput("Month", h3("Select Month"),
                  choices =unique(covid_noaa_dataset["month"]),selected = 1),
      
      selectInput("State", h3("Select state"),
                  choices =unique(covid_noaa_dataset["state_name"]),selected = 1),
      
      
      sliderInput( "tavg",
                   label = "tavg",
                   min = 1,
                   max = max(covid_noaa_dataset["state_tmax"])+20,
                   value = 30),
      
      sliderInput(inputId = "prcp",
                  label = "prcp",
                  min = -10,
                  max = max(covid_noaa_dataset["state_total_prcp"])+10,
                  value = 1)
      
    ),
    
    mainPanel(
      fluidRow(
        column(12, leafletOutput("selected_var")),
        column(12, leafletOutput("plot1"))
      )
    )
  )
)



server <- function(input, output) {
  
  output$selected_var <- renderLeaflet({
    
    df = data.frame( month = as.factor(input$Month), state_name = input$State ,
                     state_tavg = input$tavg , state_total_prcp = input$prcp )
    
    count = as.integer( predict(poisson, df, type = "response") )
    
    #paste( "you chose", a)
    map_df = covid_noaa_dataset%>% 
              filter(month == as.factor(input$Month),
                    state_name ==input$State)
    
    url = "https://urldefense.proofpoint.com/v2/url?u=https-3A__developers.google.com_public-2Ddata_docs_canonical_states-5Fcsv&d=DwIFAg&c=G2MiLlal7SXE3PeSnG8W6_JBU6FcdVjSsBSbw6gcR0U&r=B8uzIkNMhKdWydN9xY4NUSbhsqKRbTFG_gmZY3kin8Q&m=ZLDhVDaJRa8xTJd2UCndV5ZKTHV5ZrzOcRkhqHloTko&s=RJ-z_AUb_Xy-Yw9rP8euzOmNJCXWGMMWkgyuhy97A8M&e= "
    lat_long_html = read_html(url)
    
    table_lat_long_df =
      lat_long_html %>%
      html_nodes(css = "table") %>%
      first() %>%
      html_table()
    
    map_df = inner_join(map_df, table_lat_long_df, by = c("key_alpha_2" = "state" ))
    
    leaflet(data = mapStates)%>%
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
          bringToFront = TRUE)) %>%
      
      addCircleMarkers(lat =map_df$latitude.y, lng = map_df$longitude.y,
                       radius =a/100000,color = "red",popup =map_df$state_name)
    
    
    
    
  })
  
  
}

shinyApp(ui,server)