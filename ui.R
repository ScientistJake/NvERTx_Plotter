###NvERTx Shiny Plotter!

#It's good practice to have as much code outside the reactive expression as possible.
#I put as much in this top part as possible
library(ggplot2)
library(reshape2)
library(plyr)

#here we start with the shiny
library(shiny)

#this makes the buttons and input
ui <- fluidPage(
  headerPanel('NvERTx Plotting Tool!'),
  sidebarPanel(
    checkboxInput('log', 'Check for Log2', value = FALSE, width = NULL),
    textInput('gene1', 'Input NvERTx number', value = "", width = NULL, placeholder =  'e.g. NvERTx77838'),
    textInput('gene2', 'Input another number or leave blank', value = "", width = NULL, placeholder = NULL),
    textInput('gene3', 'Input another number or leave blank', value = "", width = NULL, placeholder = NULL),
    textInput('gene4', 'Input another number or leave blank', value = "", width = NULL, placeholder = NULL),
    textInput('gene5', 'Input another number or leave blank', value = "", width = NULL, placeholder = NULL),
    actionButton("do", "Evaluate!"),
    checkboxInput('returnpdf', 'output pdf?', FALSE),
    conditionalPanel(
      condition = "input.returnpdf == true",
      strong("PDF size (inches):"),
      sliderInput(inputId="w", label = "width:", min=3, max=20, value=12, width=100, ticks=F),
      sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F),
      br(),
      downloadButton('downloadPlot', 'Download Plot')
  )
  ),
  mainPanel(
    h2("Expression"),
    plotOutput("plot1"),
    h2("Average counts by hour"),
    tableOutput('table'),
    h2("Annotation"),
    tableOutput('table2')
)
)
