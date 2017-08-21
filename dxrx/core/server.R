library(shiny)
library(ggplot2)

shinyServer(function(input,output) {
    source("/home/ubuntu/git/rshiny/dxrx/core/modules/run_reports/run_reports.server.R" , local=TRUE)
    source("/home/ubuntu/git/rshiny/dxrx/core/modules/cumulative_reports/cumulative_reports.server.R" , local=TRUE)
})



