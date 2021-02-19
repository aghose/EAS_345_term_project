library(shiny)

#Run This BEFORE running the app
source("phase06.R")

ui <- source("ui.R")
server <- source("server.R")

shinyApp(ui, server)
