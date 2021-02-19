#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Mortgage data"),

    sidebarLayout(
        sidebarPanel(
          list(hr(), 
               helpText("Input the nearest-neighbor coefficeint for KNN"),
               selectInput(inputId = "k", label = "k coeffecient",
                           choices = c("k=4","k=5", "k=6", 
                                       "k=356", "k=357", "k=358"),
                           selected = "k=357")
               ),
          list(hr(), 
               helpText("Input the number of clusters for K-means clustering"),
               selectInput(inputId = "clust", label = "clusters",
                           choices = c("k=3","k=4", "k=5", 
                                       "k=6", "k=7", "k=8", "k=9"),
                           selected = "k=5")
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("tabs")
        )
    )
))
