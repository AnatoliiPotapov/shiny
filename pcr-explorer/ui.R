
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Тестирование алгоритма"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('experimentFile', 'Choose .r48 / .r96',
                accept=c('.r48,.r96'))
    ),

    # Show a plot of the generated distribution
    mainPanel(

      plotOutput("rawCurves")
      #plotOutput("ppCurves")
    )
  )
))
