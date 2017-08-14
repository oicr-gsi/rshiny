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
            sliderInput("totalReadsFailThreshold",
            "fail threshold:",
            min = 0,
            max = max(as.numeric(gsub(",","",df$PF.Reads))) * 1.10,
            value = 0)
        ),
        column(3,
            selectInput("totalReadsGroup", "Group by:",
                c("None" = "none",
                "Run" = "Run",
                "Lane" = "Lane",
                "Run and Lane" = "Run_Lane")
                
            )
            
        )
    ),
    hr(),
    headerPanel("Mean Insert Size"),
    plotOutput("insertMeanPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("insertMeanFailThreshold",
            "fail threshold:",
            min = 0,
            max = max(df$Insert_Mean),
            value = 0)
        ),
        column(3,
            selectInput("insertMeanGroup", "Group by:",
                c("None" = "none",
                "Run" = "Run",
                "Lane" = "Lane",
                "Run and Lane" = "Run_Lane")
                
            )
            
        )
    ),
    hr(),
    headerPanel("Percent Mapped"),
    plotOutput("percentMappedPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("percentMappedFailThreshold",
            "fail threshold:",
            min = 0,
            max = 100.0,
            value = 0)
        ),
        column(3,
            selectInput("percentMappedGroup", "Group by:",
                c("None" = "none",
                "Run" = "Run",
                "Lane" = "Lane",
                "Run and Lane" = "Run_Lane")
                
            )
            
        )
    ),
    hr(),
    headerPanel("Percent On Target"),
    plotOutput("percentOntPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("percentOntFailThreshold",
            "fail threshold:",
            min = 0,
            max = 100.0,
            value = 0)
        ),
        column(3,
            selectInput("percentOntGroup", "Group by:",
                c("None" = "none",
                "Run" = "Run",
                "Lane" = "Lane",
                "Run and Lane" = "Run_Lane")
                
            )
            
        )
    ),
    hr(),
    headerPanel("Mean Coverage"),
    plotOutput("meanCoveragePlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("meanCoverageFailThreshold",
            "fail threshold:",
            min = 0,
            max = max(as.numeric(gsub("x","",df$Coverage))) * 1.10,
            value = 0)
        ),
        column(3,
            selectInput("meanCoverageGroup", "Group by:",
                c("None" = "none",
                "Run" = "Run",
                "Lane" = "Lane",
                "Run and Lane" = "Run_Lane")
                
            )
            
        )
    ),
    hr()
))

