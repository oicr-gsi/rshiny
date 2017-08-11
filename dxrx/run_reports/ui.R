library(shiny)
df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
    titlePanel("DxRx Run Report Analysis"),
    hr(), 

    headerPanel("Mean Insert Size"),
    plotOutput("insertMeanPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("insertMeanThreshold",
            "fail threshold:",
            min = 1,
            max = 200,
            value = 60)
        )
    ),
    hr(),
    headerPanel("Mean Insert Size"),
    plotOutput("insertMeanPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("insertMeanThreshold",
            "fail threshold:",
            min = 1,
            max = 200,
            value = 60)
        )
    ),
    hr()
))

