
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)
library(reshape2)

source("./method/method.R")
source("./math/fit.R")
source("./plot/plot.R")

pi = 3.1415926 
  
ParseInfo <- function(data) {
  return(HTML(paste(data[1],data[2],data[3],data[4],data[5],data[6],sep="<br/>")))
}

ParseNumericData <- function(data) {
    cycle = c()
    wave1 = c()
    wave2 = c()
    unnown1 = c()
    unnown2 = c()
  for (i in (1:(length(data)-6))) {
    numericVector <- as.numeric(unlist(strsplit(data[i+6], " ")))
    cycle[i] <- numericVector[1]
    wave1[i] <- numericVector[2]
    wave2[i] <- numericVector[3]
    unnown1[i] <- numericVector[4]
    unnown2[i] <- numericVector[5]
  }
  numericData <- cbind(cycle, wave1, wave2, unnown1, unnown2)
  return(numericData)
}

server <- function(input, output) {
  
  result <- reactive({
    output<- input$rho * input$Cud * (input$ld - input$lb)**2 / 2 / parameters()[[1]][[2]] / 
      log((parameters()[[1]][[1]]),base = exp(1)) / (10 ** 6)
    return(output)
  })
  
  parameters <- reactive({
    output <- list(
      Parameters <- CalculateParameters(nls_first(), nls_second(), input$period)
    )
    return(output)
  })
  
  isFile_first <- reactive({
    inFile <- input$inputFile_first
    if (is.null(inFile))
      return(NULL)
    return(readLines(inFile$datapath))
  })
  
  isFile_second <- reactive({
    inFile <- input$inputFile_second
    if (is.null(inFile))
      return(NULL)
    return(readLines(inFile$datapath))
  })
  
  numericData_first <- reactive({
    ParseNumericData(isFile_first())
  })
  
  numericData_second <- reactive({
    ParseNumericData(isFile_second())
  })
  
  nls_first <- reactive({
    if (is.null(isFile_first())) return(NULL)
    output <- list()
    output[[1]] <- FitBaseline(numericData_first())
    output[[2]] <- FitResultLine(numericData_first())
    return(output)
  })
  
  nls_second <- reactive({
    if (is.null(isFile_second())) return(NULL)
    output <- list()
    output[[1]] <- FitBaseline(numericData_second())
    output[[2]] <- FitResultLine(numericData_second())
    return(output)
  })

  output$status <- renderPrint({
    if (is.null(isFile_first())) {
      print("Не загружен файл с данными (по ближнему положению).")
    }
    if (is.null(isFile_second())) {
      print("Не загружен файл с данными (по дальнему положению).")
    }
    if (!is.null(isFile_first()) && !is.null(isFile_second())) {
      print("OK")
    }
  })
  
  output$file_input <- renderUI({
    box(
      title = "Загрузка файлов .dat:",
      width = 12,
      fileInput('inputFile_first', 'Загрузка файла (по ближнему положению)',
                accept=c('.dat')),
      fileInput('inputFile_second', 'Загрузка файла (по дальнему положению)',
                accept=c('.dat')),
      h4("Статус: "),
      textOutput('status')
    )
  })

  output$plot <- renderUI({
    if  (!is.null(isFile_first()) && !is.null(isFile_second())) {
      data_first <- numericData_first()
      data_second <- numericData_second()
      box(
        title = "Графики фитирования кривых:",
        "По ближнему положению:",
        hr(),
        renderPlot(plot_ggplot(data_first, nls_first())),
        hr(),
        "По дальнему положению:",
        hr(),
        renderPlot(plot_ggplot(data_second, nls_second()))
      )
    }
  })
  
  output$data <- renderUI({
    if  (!is.null(isFile_first()) && !is.null(isFile_second())) {
      data_first <- numericData_first()
      data_second <- numericData_second()
      box(
        title = "Параметры фитирования",
        "По ближнему положению:",
        hr(),
        renderPrint(summary(nls_first()[[2]])),
        hr(),
        "По дальнему положению:",
        hr(),
        renderPrint(summary(nls_second()[[2]]))
      )
    }
  })
  
  output$markdown <- renderUI({
    if  (!is.null(isFile_first()) && !is.null(isFile_second())) {
      box(
      title = "Результаты измерений:",
      width =12,
      includeMarkdown("./markdown/info.Rmd"),
      hr(),
      h3("Теплопроводность исследуемого образца, Ватт/ (метр*Кельвин):"),
      renderPrint(print(result())),
      hr(),
      h4("Параметры:"),
      renderPrint(print(parameters())),
      h4("Параметры фитирования задающих кривых:"),
      renderPrint(summary(nls_first()[[1]])),
      renderPrint(summary(nls_second()[[1]]))
      )
    }
  })
  
  output$settings <- renderUI({
    box(
      width =12,
      h4("Параметры образца"),
      numericInput("period", label = h5("Период, С:"), value = 100),
      numericInput("rho", label = h5("Плотность образца, kg/m^3:"), value = 6000),
      numericInput("Cud", label = h5("Удельная теплоемкость, Дж/(kg*K):"), value = 500),
      numericInput("lb", label = h5("Расстояние по ближнему положению, mm:"), value = 15),
      numericInput("ld", label = h5("Расстояние по дальнему положению, mm:"), value = 25)
    )
  })
}
