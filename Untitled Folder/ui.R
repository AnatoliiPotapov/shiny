
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Обработка данных стенда измерения теплопроводности", titleWidth = 1000),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      uiOutput('file_input'),
      uiOutput('settings'),
      uiOutput('markdown'),
      uiOutput('plot'),
      uiOutput('data')
    )
    
  )
)