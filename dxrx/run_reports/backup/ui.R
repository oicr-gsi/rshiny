library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel("DxRx Run Report Analysis"),
  hr(),
  
  headerPanel("Percent Mapped on Target"),
  plotOutput("percentMappedOnTargetPlot"),
  hr(),
  fluidRow(
    column(3,
           
      sliderInput("percentMappedOnTargetThreshold",
                "fail threshold:",
                min = 1,
                max = 100,
                value = 60)
    )
  ),
  
  hr(),
  
  headerPanel("Map Percent"),
  plotOutput("mapPercentPlot"),
  hr(),
  hr(),
  fluidRow(
    column(3,
           
           sliderInput("mapPercentThreshold",
                       "fail threshold:",
                       min = 1,
                       max = 100,
                       value = 90)
    )
  )
  
  
))
