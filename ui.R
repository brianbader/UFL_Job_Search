library(shiny)
library(DT)


shinyUI(list(basicPage(
    h2('The UFL jobs data'),
    dataTableOutput('mytable')
  )))

