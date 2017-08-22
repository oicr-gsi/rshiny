library(shiny)
library(ggplot2)

shinyServer(function(input,output) {
    source("VAR_INSTANCE_DIR/modules/run_reports/run_reports.server.R" , local=TRUE)
    source("VAR_INSTANCE_DIR/modules/cumulative_reports/cumulative_reports.server.R" , local=TRUE)
})



