library(shiny)
library(ggplot2)

shinyServer(function(input,output) {
    source("modules/run_reports/run_reports.server.R" , local=TRUE)
})