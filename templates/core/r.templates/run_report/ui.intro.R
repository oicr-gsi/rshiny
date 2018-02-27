library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table(".DATA.DIR./run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(
    navbarPage(
        ".PROJECT. Run Reports",
        navbarMenu("Plots",
