#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Mortgage loans data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "file",
                "Upload the file",
                multiple = TRUE,
                accept = NULL,
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            ),
            helpText("Default max. file size is 45MB"),
            uiOutput("selectfile"),
            uiOutput("selectKNN")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("tb")
        )
    )
))
