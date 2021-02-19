#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {

    output$knn <- renderPlot({
      if(input$k == "k=358"){plot(knn_model_358)}
      if(input$k == "k=357"){plot(knn_model_357)}
      if(input$k == "k=356"){plot(knn_model_356)}
      
      if(input$k == "k=6"){plot(knn_model_6)}
      if(input$k == "k=5"){plot(knn_model_5)}
      if(input$k == "k=4"){plot(knn_model_4)}

    })
    
    output$knn_tbl <- renderPlot({
      if(input$k == "k=358"){plot(tbl_358)}
      if(input$k == "k=357"){plot(tbl_357)}
      if(input$k == "k=356"){plot(tbl_356)}
      
      if(input$k == "k=6"){plot(tbl_6)}
      if(input$k == "k=5"){plot(tbl_5)}
      if(input$k == "k=4"){plot(tbl_4)}
      
    })
    
    output$knn_cmtx <- renderPrint({
      if(input$k == "k=358"){confusionMatrix(tbl_358)}
      else if(input$k == "k=357"){confusionMatrix(tbl_357)}
      else if(input$k == "k=356"){confusionMatrix(tbl_356)}
      
      else if(input$k == "k=6"){confusionMatrix(tbl_6)}
      else if(input$k == "k=5"){confusionMatrix(tbl_5)}
      else if(input$k == "k=4"){confusionMatrix(tbl_4)}
      
    })
    
    output$kmeans_cntrs <- renderPrint({
      if(input$clust == "k=3"){kmeans_model_03$centers}
      else if(input$clust == "k=4"){kmeans_model_04$centers}
      else if(input$clust == "k=5"){kmeans_model_05$centers}
      else if(input$clust == "k=6"){kmeans_model_06$centers}
      else if(input$clust == "k=7"){kmeans_model_07$centers}
      else if(input$clust == "k=8"){kmeans_model_08$centers}
      else if(input$clust == "k=9"){kmeans_model_09$centers}
    })
    
    output$kmeans_goodness <- renderPrint({
      if(input$clust == "k=3"){kmeans_model_03$betweenss/kmeans_model_03$totss}
      else if(input$clust == "k=4"){kmeans_model_04$betweenss/kmeans_model_04$totss}
      else if(input$clust == "k=5"){kmeans_model_05$betweenss/kmeans_model_05$totss}
      else if(input$clust == "k=6"){kmeans_model_06$betweenss/kmeans_model_06$totss}
      else if(input$clust == "k=7"){kmeans_model_07$betweenss/kmeans_model_07$totss}
      else if(input$clust == "k=8"){kmeans_model_08$betweenss/kmeans_model_08$totss}
      else if(input$clust == "k=9"){kmeans_model_09$betweenss/kmeans_model_09$totss}
    })
    
    output$tabs <- renderUI(
      tabsetPanel(
        tabPanel("KNN model outputs", 
                 plotOutput("knn"),
                 plotOutput("knn_tbl"),
                 verbatimTextOutput("knn_cmtx")),
        tabPanel("K-means model outputs",
                 verbatimTextOutput("kmeans_cntrs"),
                 verbatimTextOutput("kmeans_goodness"))
      )
    )

})
