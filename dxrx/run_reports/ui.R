library(shiny)
df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
    titlePanel("DxRx Run Report Analysis"),
    hr(), 

    headerPanel("Total Reads (Pass filter)"),
    plotOutput("totalReadsPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("totalReadsThreshold",
            "fail threshold:",
            min = 0,
            max = max(as.numeric(gsub(",","",df$PF.Reads))) * 1.10,
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
            min = 0,
            max = max(df$Insert_Mean),
            value = 60)
        )
    ),
    hr(),
    headerPanel("Percent Mapped"),
    plotOutput("percentMappedPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("percentMappedThreshold",
            "fail threshold:",
            min = 0,
            max = max(as.numeric(gsub("%","",df$Map.Percent))) * 1.10,
            value = 60)
        )
    ),
    hr()
))

