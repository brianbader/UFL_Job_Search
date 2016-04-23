library(shiny)
library(DT)


shinyUI(list(basicPage(
    h2('University of Florida Statistics Jobs Board'),
    dataTableOutput('mytable')
  )))

