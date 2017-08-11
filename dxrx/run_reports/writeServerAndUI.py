

#LIST of keys for dictionary of plot parameters
plots_L = [
    "insert-mean"
    #"percent-mapped",
    #"percent-on-target",
    #"mean-coverage",
    #"percent-covered-at-8x-or-higher",
    #"minimum-coverage-for-90-percent-of-target"
]

#sRL: code for server.R app
#uRL: cod for ui.R app
sRL = []
uRL = []

#DICTIONARY containing the parameters for the various barplots that will be produced through ggplot2
plots_D = {
    "insert-mean" : {
        "plot_variable" : "insertMeanPlot",
        "threshold_variable" : "insertMeanThreshold",
        "r_column" : "Insert_Mean",
        "text_to_remove" : "None",
        "y_label" : "mean insert size",
        
        "ui_title" : "Mean Insert Size",
        "ui_default_threshold" : "75",
    }
    
#    "percent-mapped" : {
#        "output_image" : plot_dir+"/00-percent-mapped.png",
#        "column_header" : "Mapped %",
#        "text_to_remove" : "%",
#        "threshold" : "90",
#        "y_label" : "% reads mapped to reference"
#    },
    
#    "percent-on-target" : {
#        "output_image" : plot_dir+"/01-percent-on-target.png",
#        "r_column" : "Reads.on.Target..",
#        "column_header" : "Reads on Target %",
#        "text_to_remove" : "%",
#        "threshold" : "60",
#        "y_label" : "% reads ONT"
#    },
        
#    "mean-coverage" : {
#        "output_image" : plot_dir+"/02-mean-coverage.png",
#        "r_column" : "Aver..Coverage",
#        "column_header" : "Aver. Coverage",
#        "text_to_remove" : "x",
#        "threshold" : "50",
#        "y_label" : "mean coverage"
#    },
        
#    "percent-covered-at-8x-or-higher" : {
#        "output_image" : plot_dir+"/03-percent-covered-at-8x-or-higher.png",
#        "r_column" : "Base.coverage.at.8x",
#        "column_header" : "Base coverage at 8x",
#        "text_to_remove" : "%",
#        "threshold" : "90",
#        "y_label" : "percent of target bases covered at 8x or higher"
#    },
        
#    "minimum-coverage-for-90-percent-of-target" : {
#        "output_image" : plot_dir+"/04-90-percent-of-target-covered-at-minimum.png",
#        "r_column" : "X90..covered.at",
#        "column_header" : "90% covered at",
#        "text_to_remove" : "x",
#        "threshold" : "8",
#        "y_label" : "minimum level of coverage for 90% of target region"
#    }
    
}

###BEGINNING OF SERVER R CODE

###INITIAL RSHINY SERVER R CODE
sr = """library(shiny)
library(ggplot2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyServer(function(input, output) {
"""
sRL.append(sr)



###PER PLOT RSHINY SERVER R CODE
for plot_key in plots_L:
    pD = plots_D[plot_key]
    
    sr = """output$%s <- renderPlot({
myseq <- seq(from=1,to=nrow(df))
df$record <- myseq

t <- input$%s
""" % (pD["plot_variable"] , pD["threshold_variable"])
    sRL.append(sr)
    
    sr = ""
    if pD["text_to_remove"] == "None":
        sr = '''d <- data.frame(df$Library, df$record, df$%s)''' % (pD["r_column"])
    else:
        sr = '''d <- data.frame(df$Library , df$record, as.numeric(gsub("%s","",df$%s)))''' % (pD["text_to_remove"] , pD["r_column"])
    sRL.append(sr)
    
    sr = """d$group <- as.factor((d[,3] > t)*1)
    colnames(d) <- c("sample","library","my_y","threshold")
    
    nFail <- nrow(d[which(d[,4] == 0),])
    
    if (nFail == 0) {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="%s") + scale_y_continuous(limits=c(0.0,200.0)) +
        geom_hline(yintercept=t, color="red", linetype="dashed") +
        geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
        theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
        scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
      
    } else {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all", y="%s") + scale_y_continuous(limits=c(0.0,200.0)) +
        geom_hline(yintercept=t, color="red", linetype="dashed") +
        geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90) +
        geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
        theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
        scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    }  
    
    
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
df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyUI(fluidPage(
    titlePanel("DxRx Run Report Analysis),
    hr(), 
"""
uRL.append(ur)

###PER PLOT RSHINY UI R CODE
for plot_key in plots_L: 
    ur = """headerPanel("%s"),
plotOutput("%s"),
hr(),
fluidRow(
    column(3,
        sliderInput("%s",
        "fail threshold:",
        min = 1,
        max = 200,
        value = 60)
    )
),
hr(),

""" % (pD["ui_title"] , pD["plot_variable"] , pD["threshold_variable"])
uRL.append(ur)

uRL[-1] = uRL[-1][:-1]

#ENDING RSHINY UI R CODE
ur = """))
"""
uRL.append(ur)

print "\n".join(sRL)
print "**********"
print "\n".join(uRL)


