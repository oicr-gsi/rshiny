

#LIST of keys for dictionary of plot parameters
plots_L = [
    "total-reads",
    "insert-mean"
    #"percent-mapped"
    #"percent-on-target",
    #"mean-coverage"
]

#sRL: code for server.R app
#uRL: cod for ui.R app
sRL = []
uRL = []

#DICTIONARY containing the parameters for the various barplots that will be produced through ggplot2
plots_D = {
    "total-reads" : {
        "plot_variable" : "totalReadsPlot",
        "threshold_variable" : "totalReadsThreshold",
        "r_column" : "PF.Reads",
        "text_to_remove" : ",",
        "y_label" : "total reads (pass filter)",
        
        "ui_title" : "Total Reads (Pass filter)",
        "ui_default_threshold" : "0"
    },  
    
    "insert-mean" : {
        "plot_variable" : "insertMeanPlot",
        "threshold_variable" : "insertMeanThreshold",
        "r_column" : "Insert_Mean",
        "text_to_remove" : "None",
        "y_label" : "mean insert size",
        
        "ui_title" : "Mean Insert Size",
        "ui_default_threshold" : "0"
    },
    
    "percent-mapped" : {
        "plot_variable" : "percentMappedPlot",
        "threshold_variable" : "percentMappedThreshold",
        "r_column" : "Map.Percent",
        "text_to_remove" : "%",
        "y_label" : "percent of reads mapped to hg19",
        
        "ui_title" : "Percent Mapped",
        "ui_default_threshold" : "0"
    },
    
    "percent-on-target" : {
        "plot_variable" : "percentOntPlot",
        "threshold_variable" : "percentOntThreshold",
        "r_column" : "Percent.mapped.on.Target",
        "text_to_remove" : "%",
        "y_label" : "percent of mapped reads on target",
        
        "ui_title" : "Percent On Target",
        "ui_default_threshold" : "0"
    },
        
    "mean-coverage" : {
        "plot_variable" : "meanCoveragePlot",
        "threshold_variable" : "meanCoverageThreshold",
        "r_column" : "Coverage",
        "text_to_remove" : "x",
        "y_label" : "mean coverage",
        
        "ui_title" : "Mean Coverage",
        "ui_default_threshold" : "0"
    },
}

###BEGINNING OF SERVER R CODE

###INITIAL RSHINY SERVER R CODE
sr = """library(shiny)
library(ggplot2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\\t")

shinyServer(function(input, output) {
"""
sRL.append(sr)



###PER PLOT RSHINY SERVER R CODE
for plot_key in plots_L:
    pD = plots_D[plot_key]
    
    sr = """    output$%s <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))
    df$record <- myseq

    t <- input$%s
""" % (pD["plot_variable"] , pD["threshold_variable"])
    sRL.append(sr)
    
    sr = ""
    if pD["text_to_remove"] == "None":
        sr = '''    d <- data.frame(df$Library, df$record, df$%s)''' % (pD["r_column"])
    else:
        sr = '''    d <- data.frame(df$Library , df$record, as.numeric(gsub("%s","",df$%s)))''' % (pD["text_to_remove"] , pD["r_column"])
    sRL.append(sr)
    
    sr = """    d$group <- as.factor((d[,3] > t)*1)
    colnames(d) <- c("sample","library","my_y","threshold")
    
    windowHeight <- max(d$my_y) * 1.10
    
    nFail <- nrow(d[which(d[,4] == 0),])
    
    if (nFail == 0) {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="%s") + scale_y_continuous(limits=c(0.0,windowHeight)) +
        geom_hline(yintercept=t, color="red", linetype="dashed") +
        geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
        theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
        scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
      
    } else {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="%s") + scale_y_continuous(limits=c(0.0,windowHeight)) +
        geom_hline(yintercept=t, color="red", linetype="dashed") +
        geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90) +
        geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
        theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
        scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    }  
    
    
    })""" % (pD["y_label"] , pD["y_label"])
    sRL.append(sr)



#ENDING RSHINY SERVER R CODE
sr = """})
"""
sRL.append(sr)


#END OF RSHINY SERVER R CODE



#BEGINNING OF RSHINY UI R CODE

###INITIAL RSHINY UI R CODE
ur = """library(shiny)
df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\\t")

shinyUI(fluidPage(
    titlePanel("DxRx Run Report Analysis"),
    hr(), 
"""
uRL.append(ur)

###PER PLOT RSHINY UI R CODE
for plot_key in plots_L:
    pD = plots_D[plot_key]
    
    fail_slider_max_string = ""
    if pD["text_to_remove"] == "None":
        fail_slider_max_string = '''max(df$%s)''' % (pD["r_column"])
    else:
        fail_slider_max_string = '''max(as.numeric(gsub(",","",df$%s))) * 1.10''' % (pD["r_column"])
    
    ur = """    headerPanel("%s"),
    plotOutput("%s"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("%s",
            "fail threshold:",
            min = 0,
            max = %s,
            value = 60)
        )
    ),
    hr(),""" % (pD["ui_title"] , pD["plot_variable"] , pD["threshold_variable"] , fail_slider_max_string)
    uRL.append(ur)

toreplace = uRL[-1]
uRL.pop()
uRL.append(toreplace[:-1])

#ENDING RSHINY UI R CODE
ur = """))
"""
uRL.append(ur)

sROut = "server.R"
open(sROut,"w").write("\n".join(sRL)+"\n")

uROut = "ui.R"
open(uROut,"w").write("\n".join(uRL)+"\n")
