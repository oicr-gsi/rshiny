library(shiny)

shinyUI(
    navbarPage(theme="VAR_INSTANCE_DIR/www/styles.css",
        "VAR_PROJECT shiny server - core QC",
        tabPanel("About",
            helpText(
                h2("VAR_PROJECT shiny server - core QC"),
                h3("Overview"),
                "This is the landing page for interactive visualization and reporting of QC stats for the VAR_PROJECT project.",
                h3("Modules"),
                h4("Run Reports"),
                "library-level stats compiled from html run reports",
                h4("Cumulative Reports"),
                "sample-level stats compiled from project cumulative report" 
            )  
        ),
        navbarMenu("Modules",
            tabPanel("Run Reports",
                source("VAR_INSTANCE_DIR/modules/run_reports/run_reports.ui.R")
            ),
            tabPanel("Cumulative Reports",
                source("VAR_INSTANCE_DIR/modules/cumulative_reports/cumulative_reports.ui.R")
            )
        )
    )
)

