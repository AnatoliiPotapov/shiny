
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(chipPCR)
source("./R/readDT.R")


shinyServer(function(input, output) {

  experimentFile <- reactive({
    
    inFile <- input$experimentFile
    
    if (is.null(inFile))
      return(NULL)
    
    read_DT(inFile$name, inFile$datapath)
  
  })
  
  
  output$rawCurves <- renderPlot({
    
    plotCurves(1:50 , experimentFile()$optic$FAM_2, one.plot = TRUE)

  })
  
  output$ppCurves <- renderPlot({
    
    plotCurves(1:50 , experimentFile()$optic$FAM_2, CPP = TRUE)
    
  })

})
