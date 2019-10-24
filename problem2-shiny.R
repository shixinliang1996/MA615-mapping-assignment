library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)

#data <- read.csv( "earthquakes_1.0_month.csv", header = TRUE )
data <- read.csv("Colleges and universities in Boston.csv", header = TRUE)

#categorize earthquake depth
# data$depth_type <- ifelse(data$depth <= 70, "shallow", 
#                           ifelse(data$depth <= 300 | data$depth >70,
#                                  "intermediate", ifelse(data$depth > 300, "deep", "other")))

ui <- fluidPage(
  mainPanel( 
    #this will create a space to display our map
    leafletOutput(outputId = "mymap"), 
    #put the checkmarks ontop of the map
    absolutePanel(top = 60, left = 20, 
                  checkboxInput("markers", "Depth", FALSE),
                  checkboxInput("heat", "Heatmap", FALSE)
    )
  ))

server <- function(input, output, session) {
  # #define the color pallate for the magnitidue of the earthquake
  # pal <- colorNumeric(
  #   palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
  #   domain = data$mag)
  # 
  # #define the color of for the depth of the earquakes
  # pal2 <- colorFactor(
  #   palette = c('blue', 'yellow', 'red'),
  #   domain = data$depth_type
  # )
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data = data) %>%
      setView(-71.10000, 42.32000, zoom = 11) %>% 
      addProviderTiles("CartoDB.Positron", group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
      addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
      addMarkers(~Longitude, ~Latitude, label = ~Name, group = "Universities") %>% 
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Map", "Satellite", "Relief"),
        overlayGroups = c("Universities"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  # observe({
  #   proxy <- leafletProxy("mymap", data = data)
  #   proxy %>% clearMarkers()
  #   if (input$markers) {
  #     proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,      label = ~as.character(paste0("Magnitude: ", sep = " ", mag))) %>%
  #       addLegend("bottomright", pal = pal2, values = data$depth_type,
  #                 title = "Depth Type",
  #                 opacity = 1)}
  #   else {
  #     proxy %>% clearMarkers() %>% clearControls()
  #   }
  # })
  
  # observe({
  #   proxy <- leafletProxy("mymap", data = data)
  #   proxy %>% clearMarkers()
  #   if (input$heat) {
  #     proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~mag, blur =  10, max = 0.05, radius = 15) 
  #   }
  #   else{
  #     proxy %>% clearHeatmap()
  #   }
  #   
  #   
  # })
  
}

shinyApp(ui, server)
