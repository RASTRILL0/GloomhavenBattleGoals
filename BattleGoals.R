library(shiny)
library(shinyjs)
library(shinythemes)
library(tibble)
library(dplyr)
#`%then%` <- shiny:::`%OR%`


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'url')
