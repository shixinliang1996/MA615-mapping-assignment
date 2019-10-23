library(shiny)
library(ggmap)
library(maptools)
library(maps)

mapWorld <- map_data("world")

mp1 <- ggplot(mapWorld, aes(x=long, y=lat, group=group))+
  geom_polygon(fill="white", color="black") +
  coord_map(xlim=c(-180,180), ylim=c(-60, 90))

mp2 <- mp1 + coord_map("cylindrical",xlim=c(-180,180), ylim=c(-60, 90))

mp3 <- mp1 + coord_map("mercator",xlim=c(-180,180), ylim=c(-60, 90))

mp4 <- mp1 + coord_map("sinusoidal", xlim=c(-180,180), ylim=c(-60, 90))

mp5 <- mp1 + coord_map("gnomonic", xlim=c(-180,180), ylim=c(-60, 90))

mp6 <- mp1 + coord_map("rectangular", parameters = 0, xlim=c(-180,180), ylim=c(-60, 90))

mp7 <- mp1 + coord_map("cylequalarea", parameters = 0, xlim=c(-180,180), ylim=c(-60, 90))


##############################################
ui <- fluidPage(
  selectInput("dataset", label = "World Map Type", choices = c("original"="p1", "cylindrical"="p2", "mercator"="p3", "sinusoidal"="p4",
                                                               "gnomonic"="p5", "rectangular"="p6", "cylequalarea"="p7"),
              selected = "p1"), 
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    if (input$dataset=="p1") return(mp1)
    else if (input$dataset=="p2") return(mp2)
    else if (input$dataset=="p3") return(mp3)
    else if (input$dataset=="p4") return(mp4)
    else if (input$dataset=="p5") return(mp5)
    else if (input$dataset=="p6") return(mp6)
    else if (input$dataset=="p7") return(mp7)
  })
}

shinyApp(ui, server)
