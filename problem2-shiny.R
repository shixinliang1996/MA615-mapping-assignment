library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)

#data <- read.csv( "earthquakes_1.0_month.csv", header = TRUE )
data <- read.csv("Colleges and universities in Boston.csv", header = TRUE)

#categorize university's student number
data$NumStudent[data$NumStudent == 0] <- NA
data$NumStudent[data$NumStudent <= 1000] <- "small"
data$NumStudent[data$NumStudent < 5000] <- "medium"
data$NumStudent[data$NumStudent == 9148] <- "large"
data$NumStudent[data$NumStudent == 5003] <- "large"

data$YearBuilt[data$YearBuilt == 0] <- NA
data$YearBuilt[data$YearBuilt < 1900] <- "1800s"
data$YearBuilt[data$YearBuilt >= 1900] <- "1900s"

ui <- fluidPage(
  mainPanel( 
    #this will create a space to display our map
    leafletOutput(outputId = "mymap"), 
    textOutput("information"),
    #put the checkmarks ontop of the map
    absolutePanel(top = 80, left = 20, 
                  checkboxInput("markers", "Number of Student", FALSE),
                  checkboxInput("year", "Year Built", FALSE)
    )
  ))

server <- function(input, output, session) {
  #define the color pallate for the magnitidue of the earthquake
  pal1 <- colorFactor(
    palette = c('blue', 'pink'),
    domain = data$YearBuilt)
   
  #define the color of for the depth of the earquakes
  pal2 <- colorFactor(
    palette = c('red', 'orange', 'yellow'),
    domain = data$NumStudent
  )
  
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
  
  output$information <- renderText("We can see that most universities locate near Charles River.  
                                   Schools that built in 1800s are more likely to be near Charles River while schools that built in 1900s are a little bit far from the center of Boston.  
                                   And schools with large number of student locate in downtown area: near Boston and Fenway.")
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>% addCircleMarkers(stroke = TRUE, color = ~pal2(NumStudent), fillOpacity = 0.2,      label = ~as.character(paste0("Number of Student: ", sep = " ", NumStudent))) %>%
        addLegend("bottomright", pal = pal2, values = data$NumStudent,
                  title = "Number of Studen",
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$year) {
      proxy %>% addCircleMarkers(stroke = TRUE, color = ~pal1(YearBuilt), fillOpacity = 0.2,      label = ~as.character(paste0("Year Built: ", sep = " ", YearBuilt))) %>%
        addLegend("bottomright", pal = pal1, values = data$YearBuilt,
                  title = "Year Built",
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })

}

shinyApp(ui, server)
