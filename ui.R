library(shiny)
library(DT)

shinyUI(list(basicPage(
  h2('University of Florida Statistics Jobs Board'),
  br(),
  actionButton("refresh", "Update List"),
  br(),
  br(),
  dataTableOutput('mytable')
)))