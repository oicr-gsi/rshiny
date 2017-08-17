

#LIST of keys for dictionary of plot parameters
plots_L = [
    "total-reads",
    "insert-mean",
    "percent-mapped",
    "percent-on-target",
    "mean-coverage"
]

#sRL: code for server.R app
#uRL: cod for ui.R app
sRL = []
uRL = []

#DICTIONARY containing the parameters for the various barplots that will be produced through ggplot2
plots_D = {
    "total-reads" : {
        "rvar_prefix" : "totalReads",
        "plot_variable" : "totalReadsPlot",
        "threshold_variable" : "totalReadsThreshold",
        "r_column" : "PF.Reads",
        "text_to_remove" : ",",
        "y_label" : "total reads (pass filter)",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Total Reads (Pass filter)"
    },  
    
    "insert-mean" : {
        "rvar_prefix" : "insertMean",
        "plot_variable" : "insertMeanPlot",
        "threshold_variable" : "insertMeanThreshold",
        "r_column" : "Insert_Mean",
        "text_to_remove" : "None",
        "y_label" : "mean insert size",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Mean Insert Size"
    },
    
    "percent-mapped" : {
        "rvar_prefix" : "percentMapped",
        "plot_variable" : "percentMappedPlot",
        "threshold_variable" : "percentMappedThreshold",
        "r_column" : "Map.Percent",
        "text_to_remove" : "%",
        "y_label" : "percent of reads mapped to hg19",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent Mapped"
    },
    
    "percent-on-target" : {
        "rvar_prefix" : "percentOnt",
        "plot_variable" : "percentOntPlot",
        "threshold_variable" : "percentOntThreshold",
        "r_column" : "Percent.mapped.on.Target",
        "text_to_remove" : "%",
        "y_label" : "percent of mapped reads on target",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent On Target"
    },
        
    "mean-coverage" : {
        "rvar_prefix" : "meanCoverage",
        "plot_variable" : "meanCoveragePlot",
        "threshold_variable" : "meanCoverageThreshold",
        "r_column" : "Coverage",
        "text_to_remove" : "x",
        "y_label" : "mean coverage",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Mean Coverage"
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

shinyServer(function(input, output) {

df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\\t")
"""

sRL.append(sr)


###PER PLOT RSHINY SERVER R CODE
for plot_key in plots_L:
    pD = plots_D[plot_key]
    
    sr = """    output$%sPlot <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))

    t <- input$%sFailThreshold
""" % (pD["rvar_prefix"] , pD["rvar_prefix"])
    sRL.append(sr)
    
    
    sr = """sortby <- input$%sGroup
    if (sortby != "none") {
        df <- df[order(df[[sortby]]),]
    }
""" % (pD["rvar_prefix"])
    sRL.append(sr)
    
    sr = "df$record <- myseq"
    sRL.append(sr)
    
    sr = ""
    if pD["text_to_remove"] == "None":
        sr = '''    d <- data.frame(df$Library, df$record, df$%s)''' % (pD["r_column"])
    else:
        sr = '''    d <- data.frame(df$Library , df$record, as.numeric(gsub("%s","",df$%s)))''' % (pD["text_to_remove"] , pD["r_column"])
    sRL.append(sr)
    
    sr = """d$group <- as.factor((d[,3] > t)*1)
    
    
    colnames(d) <- c("sample","library","my_y","threshold")"""
    sRL.append(sr)
    
    #df$record <- myseq
    
    sr=""
    if pD["window_height_type"] == "dynamic":
        sr = "windowHeight <- max(d$my_y) * 1.10"    
    elif pD["window_height_type"] == "fixed":
        sr = "windowHeight <- %s" % (pD["window_height_max"])
    sRL.append(sr)
    
    sr="""plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="%s") + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2)
    
    
    
    unique.groupby <- unique(df[[sortby]])
    
    if (sortby != "none") {
        for (unique.group in unique.groupby){
            group.lines <- df[which(df[[sortby]] == unique.group),]
            first.record <- head(group.lines , n=1)$record
            last.record <- tail(group.lines , n=1)$record
    
            plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
        }
    }
    
    
    
    nFail <- nrow(d[which(d[,4] == 0),])
    if (nFail > 0) {
        plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90)
    }
      
    plot <- plot + theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4))
    plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    plot
        
        
      
    
    
    })""" % (pD["y_label"])
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

shinyUI(fluidPage( theme = "styles.css",
    headerPanel("DxRx Run Report Analysis"),
    hr(), 
"""
uRL.append(ur)

###PER PLOT RSHINY UI R CODE
for plot_key in plots_L:
    pD = plots_D[plot_key]
    
    fail_slider_max_string = ""
    if pD["window_height_type"] == "dynamic":
        if pD["text_to_remove"] == "None":
            fail_slider_max_string = '''max(df$%s)''' % (pD["r_column"])
        else:
            fail_slider_max_string = '''max(as.numeric(gsub("%s","",df$%s))) * 1.10''' % (pD["text_to_remove"] , pD["r_column"])
            
    elif pD["window_height_type"] == "fixed":
        fail_slider_max_string = pD["window_height_max"]
    
    
    
    ur = """    titlePanel("%s"),
    plotOutput("%sPlot"),
    hr(),
    fluidRow(
        column(3,
            sliderInput("%sFailThreshold",
            "fail threshold:",
            min = 0,
            max = %s,
            value = 0)
        ),
        column(3,
            selectInput("%sGroup", "Group by:",
                c("None" = "none",
                "Run" = "Run",
                "Lane" = "Lane",
                "Run and Lane" = "Run_Lane")
                
            )
            
        )
    ),
    hr(),""" % (pD["ui_title"] , pD["rvar_prefix"] , \
                pD["rvar_prefix"] , fail_slider_max_string,\
                pD["rvar_prefix"] )
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
