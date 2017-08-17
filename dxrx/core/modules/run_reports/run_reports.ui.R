library(shiny)
df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
	headerPanel("DxRx Run Report Analysis"),
	hr(), 

	titlePanel("Total Reads (Pass filter)"),
	plotOutput("runReports_totalReadsPlot"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_totalReadsFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub(",","",df$PF.Reads))) * 1.10,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_totalReadsGroup", "Group by:",
				c("None" = "none",
				"Run" = "Run",
				"Lane" = "Lane",
				"Run and Lane" = "Run_Lane")
			)
		)
	),
	hr(),
	titlePanel("Mean Insert Size"),
	plotOutput("runReports_insertMeanPlot"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_insertMeanFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(df$Insert_Mean),
				value = 0
			)
		),
		column(3,
			selectInput("runReports_insertMeanGroup", "Group by:",
				c("None" = "none",
				"Run" = "Run",
				"Lane" = "Lane",
				"Run and Lane" = "Run_Lane")
			)
		)
	),
	hr(),
	titlePanel("Percent Mapped"),
	plotOutput("runReports_percentMappedPlot"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_percentMappedFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_percentMappedGroup", "Group by:",
				c("None" = "none",
				"Run" = "Run",
				"Lane" = "Lane",
				"Run and Lane" = "Run_Lane")
			)
		)
	),
	hr(),
	titlePanel("Percent On Target"),
	plotOutput("runReports_percentOntPlot"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_percentOntFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_percentOntGroup", "Group by:",
				c("None" = "none",
				"Run" = "Run",
				"Lane" = "Lane",
				"Run and Lane" = "Run_Lane")
			)
		)
	),
	hr(),
	titlePanel("Mean Coverage"),
	plotOutput("runReports_meanCoveragePlot"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("runReports_meanCoverageFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub("x","",df$Coverage))) * 1.10,
				value = 0
			)
		),
		column(3,
			selectInput("runReports_meanCoverageGroup", "Group by:",
				c("None" = "none",
				"Run" = "Run",
				"Lane" = "Lane",
				"Run and Lane" = "Run_Lane")
			)
		)
	),
	hr()
))

