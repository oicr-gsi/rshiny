library(shiny)
df <- read.table("/home/ubuntu/data/dxrx/core/cumulative_reports/dxrx.cumulative.report.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
	headerPanel("DxRx Cumulative Report Analysis"),
	hr(), 

	titlePanel("Total Reads (Pass filter)"),
	plotOutput("cumlativeReports_totalReadsPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_totalReadsFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub(",","",df$Total.Reads))) * 1.10,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_totalReadsGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr(),
	titlePanel("Average Read Length"),
	plotOutput("cumlativeReports_readLengthPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_readLengthFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(df$Aver..Read.Length),
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_readLengthGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr(),
	titlePanel("Percent Mapped"),
	plotOutput("cumlativeReports_percentMappedPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_percentMappedFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_percentMappedGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr(),
	titlePanel("Percent On Target"),
	plotOutput("cumlativeReports_percentOntPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_percentOntFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_percentOntGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr(),
	titlePanel("Mean Coverage"),
	plotOutput("cumlativeReports_meanCoveragePlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_meanCoverageFailThreshold",
				"fail threshold:",
				min = 0,
				max = max(as.numeric(gsub("x","",df$Aver..Coverage))) * 1.10,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_meanCoverageGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr(),
	titlePanel("Percent of Target Covered at 8x or Higher"),
	plotOutput("cumlativeReports_percentCoverageEightXPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_percentCoverageEightXFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_percentCoverageEightXGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr(),
	titlePanel("Minimum Coverage for 90% of Target"),
	plotOutput("cumlativeReports_minCoverageForNinetyPercentOfTargetPlot", height = "80vh"),
	hr(),
	fluidRow(
		column(3,
			sliderInput("cumlativeReports_minCoverageForNinetyPercentOfTargetFailThreshold",
				"fail threshold:",
				min = 0,
				max = 100.0,
				value = 0
			)
		),
		column(3,
			selectInput("cumlativeReports_minCoverageForNinetyPercentOfTargetGroup", "Group by:",
				c("None" = "none",
				"Sample" = "Sample")
			)
		)
	),
	hr()
))

