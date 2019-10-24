library(leaflet)
library(maps)
library(htmlwidgets) # To save the map as a web page.

sites <- read.csv("Colleges and universities in Boston.csv", header = TRUE)
bounds <- map('state', c('Massachusetts'), fill=TRUE, plot=FALSE)

map <- leaflet(data = sites) %>%
  setView(-71.10000, 42.32000, zoom = 11) %>% 
  addProviderTiles("CartoDB.Positron", group = "Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
  addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
  addMarkers(~Longitude, ~Latitude, label = ~Name, group = "Universities") %>% 
  addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
  addScaleBar(position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Map", "Satellite", "Relief"),
    overlayGroups = c("Universities", "States"),
    options = layersControlOptions(collapsed = FALSE)
  )
invisible(print(map))

saveWidget(map, file="Boston_Public_Library_Sites2.html", selfcontained=TRUE)
