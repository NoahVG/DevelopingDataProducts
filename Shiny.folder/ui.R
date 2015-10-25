library(rCharts)
#library(shiny)
shinyUI(fluidPage(
  titlePanel("S&P 500 Retroactive Stock Comparision - Sept 20 2009 to Sept 19 2010"),
  
  fixedRow(column(6,showOutput("StockPlot","nvd3")),
           column(3,
                  h3(" Select a stock from each of the dropdown menus and press submit. The first time you use this it may take a bit to load."),
                  uiOutput("stockA"), uiOutput("stockB"),uiOutput("stockC"),
                  actionButton("submitButton","Submit")
           )),
  #hr(),
  fixedRow(
           column(3, 
                  h4("Change in Stock Price"),
                  showOutput("ChangePlot","nvd3"))),
           fixedRow(
           column(3, 
                  h4("Percent Change in Stock Price"),
                  showOutput("ChangePercPlot","nvd3"))
  )
))
