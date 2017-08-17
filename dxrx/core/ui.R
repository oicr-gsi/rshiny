library(shiny)

shinyUI(
    navbarPage(theme="styles.css",
        "DxRx shiny server - core QC",
        tabPanel("About"),
        navbarMenu("Modules",
            tabPanel("Run Reports",
                source("modules/run_reports/run_reports.ui.R")
            )
            
        )
    )
)