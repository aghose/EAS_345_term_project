#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
require(graphics)
require(grDevices)
require(class)
library(caret)

#increases max input size to 45 MB
options(shiny.maxRequestSize = 45*1024^2)

source('phase06.R')

shinyServer(function(input, output) {

    ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
    # this reactive output display the content of the input$file dataframe
    output$filedf <- renderTable({
        if(is.null(input$file)){return ()}
        input$file # the file input data frame object that contains the file attributes
    })
    
    
    
    # Extract the file path for file
    output$filedf2 <- renderTable({
        if(is.null(input$file)){return ()}
        input$file$datapath # the file input data frame object that contains the file attributes
    })
    
    ## Below code to display the structure of the input file object
    output$fileob <- renderPrint({
        if(is.null(input$file)){return ()}
        str(input$file)
    })
    
    ## Side bar select input widget coming through renderUI()
    # Following code displays the select input widget with the list of file loaded by the user
    output$selectfile <- renderUI({
        if(is.null(input$file)) {return()}
        list(hr(), 
             helpText("Select the files for which you need to see data and summary stats"),
             helpText("Data and Summary stats are limited to 4000 items due to size issues"),
             selectInput("Select", "Select", choices=input$file$name)
        )
    })
    
    ## Summary Stats code ##
    # this reactive output contains the summary of the dataset and display the summary in table format
    output$summ <- renderPrint({
        if(is.null(input$file)){return()}
        summary(read.csv(file=input$file$datapath[input$file$name==input$Select],
                         nrows = 4000))
        })
    
    ## Dataset code ##
    # This reactive output contains the dataset and display the dataset in table format
    output$table <- renderTable({ 
        if(is.null(input$file)){return()}
        read.csv(file=input$file$datapath[input$file$name==input$Select],
                 nrows = 4000)

    })
    
    ##LM code##
    output$lm <-renderPlot(
        plot(lm_model_00)
    )
    
    ##KNN code##
    output$knn <-renderPlot(
      
    )
    
    output$debug <-renderText(
      cat("k coefficient: ", input$k, "\n"),
      #cat("k_: ", k_, "\n")
    )

    ##KNN confusion_matrix table plot##
    output$confusion_plot <-renderPlot(
        plot(tbl_00)
    )

    ##KNN confusion_matrix table plot##
    output$confusion_tbl <-renderPrint(
        confusionMatrix(tbl_00)
    )
    
    ## Side bar select input widget coming through renderUI()
    # Following code displays the select input widget with the list of KNN models available
    output$selectKNN <- renderUI({
        if(is.null(input$file)) {return()}
        
        strChoices <- c("k = 4", "k = 5", "k = 6", 
                        "k = 356", "k = 357", "k = 358")
        list(hr(), 
             helpText("Input the nearest-neighbor coefficeint for KNN"),
             
             selectInput("k", "k", choices=strChoices)
        )
    })
    
    ## MainPanel tabset renderUI code ##
    # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
    # Until the file is loaded, app will not show the tabset.
    output$tb <- renderUI({
        if(is.null(input$file)) {return()}
        else
            tabsetPanel(
                tabPanel("Input File Object", tableOutput("filedf"), 
                         verbatimTextOutput("fileob")),
                tabPanel("Dataset", tableOutput("table")),
                tabPanel("Summary Stats", verbatimTextOutput("summ")),
                #tabPanel("LM models", plotOutput("lm")),
                tabPanel("KNN model outputs", plotOutput('knn'),
                         #plotOutput("confusion_plot"),
                         #verbatimTextOutput("debug"),
                         verbatimTextOutput("confusion_tbl"))
                )
    })

})
