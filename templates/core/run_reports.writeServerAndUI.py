import yaml
import sys
import os

config_yaml = sys.argv[1]
dataMap = {}
with open('config.yaml') as f:
    # use safe_load instead load
    dataMap = yaml.safe_load(f)
    
app_dir = dataMap["app-instance"]["app_dir"]
project = dataMap["app-instance"]["project"]
    
data_dir = dataMap["app-instance"]["data_dir"]
instance_dir = dataMap["app-instance"]["instance_dir"]

#LIST of keys for dictionary of plot parameters
plots_L = [
    "total-reads",
    "insert-mean",
    "percent-mapped",
    "percent-on-target",
    "mean-coverage"
]

xyplots_L = [
    "percent-on-target-vs-total-reads"
]

#sRL: code for server.R app
#uRL: code for ui.R app
sRL = []
uRL = []

#DICTIONARY containing the parameters for the various barplots that will be produced through ggplot2
plots_D = {
    "total-reads" : {
        "rvar_prefix" : "totalReads",
        "r_column" : "PF.Reads",
        "text_to_remove" : ",",
        "y_label" : "total reads (pass filter)",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Total Reads (Pass filter)"
    },  
    
    "insert-mean" : {
        "rvar_prefix" : "insertMean",
        "r_column" : "Insert_Mean",
        "text_to_remove" : "None",
        "y_label" : "mean insert size",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Mean Insert Size"
    },
    
    "percent-mapped" : {
        "rvar_prefix" : "percentMapped",
        "r_column" : "Map.Percent",
        "text_to_remove" : "%",
        "y_label" : "percent of reads mapped to hg19",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent Mapped"
    },
    
    "percent-on-target" : {
        "rvar_prefix" : "percentOnt",
        "r_column" : "Percent.mapped.on.Target",
        "text_to_remove" : "%",
        "y_label" : "percent of mapped reads on target",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent On Target"
    },
        
    "mean-coverage" : {
        "rvar_prefix" : "meanCoverage",
        "r_column" : "Coverage",
        "text_to_remove" : "x",
        "y_label" : "mean coverage",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Mean Coverage"
    }
}

xyplots_D = {
    "percent-on-target-vs-total-reads" : {
        "rvar_prefix" : "onTargetVSTotalReads",
        
        "x_r_column" : "PF.Reads",
        "x_text_to_remove" : ",",
        "x_label" : "total reads (pass filter)",
        
        "y_r_column" : "Percent.mapped.on.Target",
        "y_text_to_remove" : "%",
        "y_label" : "percent of mapped reads on target",
        
        "window_width_type" : "dynamic",
        "window_width_max" : "na",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent on Target vs Total Reads (Pass filter)"
    }
}

tab_var = "runReports" #variable indicating the tab, so that back-end variable names do not overlap with those from other tabs

###BEGINNING OF SERVER R CODE

###INITIAL RSHINY SERVER R CODE
sr = """library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue <- function(n) {
\thues = seq(15, 375, length = n + 1)
\thcl(h = hues, l = 65, c = 100)[1:n]
}

rrdf <- read.table("%s/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\\t")
""" % (data_dir)

sRL.append(sr)


###PER PLOT RSHINY SERVER R CODE

#BAR PLOTS
for plot_key in plots_L:
    pD = plots_D[plot_key]
    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
    
    sr = """output$%sPlot <- renderPlotly({
\tmyseq <- seq(from=1,to=nrow(rrdf))
\tt <- input$%sFailThreshold
""" % (tab_plot_var , tab_plot_var)
    sRL.append(sr)
    
    
    sr = """\tsortby <- input$%sGroup
\tif (sortby != "none") {
\t\trrdf <- rrdf[order(rrdf[[sortby]]),]
\t}
""" % (tab_plot_var)
    sRL.append(sr)
    
    sr = "\trrdf$record <- myseq"
    sRL.append(sr)
    
    sr = ""
    if pD["text_to_remove"] == "None":
        sr = '''\td <- data.frame(rrdf$Library, rrdf$record, rrdf$%s)''' % (pD["r_column"])
    else:
        sr = '''\td <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("%s","",rrdf$%s)))''' % (pD["text_to_remove"] , pD["r_column"])
    sRL.append(sr)
    
    sr = """\td$group <- as.factor((d[,3] > t)*1)
\tcolnames(d) <- c("library","record","value","threshold")
\td <- data.frame(d , rrdf)"""
    sRL.append(sr)
    
    
    
    sr=""
    if pD["window_height_type"] == "dynamic":
        sr = "\twindowHeight <- max(d$value) * 1.10"    
    elif pD["window_height_type"] == "fixed":
        sr = "\twindowHeight <- %s" % (pD["window_height_max"])
    sRL.append(sr)
    
    sr="""\tplot <- ggplot(d, aes(x=record, y=value)) + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "\nRun:" , Run , "\nLane:" , Lane , "\nBarcode:",Barcode) ,fill=threshold) ) + labs(x="all libraries", y="%s") + scale_y_continuous(limits=c(0.0,windowHeight))
\tplot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
\tplot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
\tunique.groupby <- unique(rrdf[[sortby]])

\tfirst_records <- c()
\tlast_records <- c()
\tmy_cols <- c()
\ty1 <- c()
\ty2 <- c()
    
\tif (sortby != "none") {
\t\tfor (i in 1:length(unique.groupby)){
\t\t\tunique.group <- unique.groupby[i]
\t\t\tgroup.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
\t\t\tfirst.record <- head(group.lines , n=1)$record
\t\t\tlast.record <- tail(group.lines , n=1)$record
\t\t\tcolor <- (i %s 2) + 10
            
\t\t\tfirst_records[i] <- first.record
\t\t\tlast_records[i] <- last.record
\t\t\tmy_cols[i] <- color
\t\t\ty1[i] <- windowHeight * 0.98
\t\t\ty2[i] <- windowHeight
\t\t}
        
\t\tgroup.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , y1 , y2)
\t\tcolnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "y1" , "y2")
\t\tplot <- plot + geom_vline(xintercept=0.5 , color="gray", linetype="dashed")
\t\tplot <- plot + geom_vline(xintercept=last_records+0.5 , color="gray", linetype="dashed")
\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))
        
\t}
        
\tnFail <- nrow(d[which(d[,4] == 0),])
\tif (nFail > 0) {
\t\t#plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=record,y=value-2,label=sample,hjust="right"), color="black", angle=90)
\t}
      
\tplot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
\tplot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9]))
\tggplotly(plot)
})""" % (pD["y_label"] , "%"*2)
    sRL.append(sr)


#XY PLOTS
for xyplot_key in xyplots_L:
    pD = xyplots_D[xyplot_key]
    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]

    sr = """output$%sPlot <- renderPlot({
""" % (tab_plot_var)
    sRL.append(sr)
    
    sr = "\td <- data.frame(rrdf$Library, "
    
    if pD["x_text_to_remove"] == "None":
        sr = sr + '''rrdf$%s, ''' % (pD["x_r_column"])
    else:
        sr = sr + '''as.numeric(gsub("%s","",rrdf$%s)), ''' % (pD["x_text_to_remove"] , pD["x_r_column"])
        
    if pD["y_text_to_remove"] == "None":
        sr = sr + '''rrdf$%s)''' % (pD["y_r_column"])
    else:
        sr = sr + '''as.numeric(gsub("%s","",rrdf$%s)))''' % (pD["y_text_to_remove"] , pD["y_r_column"])
    sRL.append(sr)
    
    sr = """\tcolnames(d) <- c("library","my_x","my_y")"""
    sRL.append(sr)
    
    
    sr=""
    if pD["window_width_type"] == "dynamic":
        sr = "\twindowWidth <- max(d$my_x) * 1.10"    
    elif pD["window_width_type"] == "fixed":
        sr = "\twindowWidth <- %s" % (pD["window_width_max"])
    sRL.append(sr)
    
    sr=""
    if pD["window_height_type"] == "dynamic":
        sr = "\twindowHeight <- max(d$my_y) * 1.10"    
    elif pD["window_height_type"] == "fixed":
        sr = "\twindowHeight <- %s" % (pD["window_height_max"])
    sRL.append(sr)
    
    sr="""\tplot <- ggplot(d, aes(x=my_x, y=my_y)) + geom_point() + labs(x="%s", y="%s") + scale_x_continuous(limits=c(0.0,windowWidth)) + scale_y_continuous(limits=c(0.0,windowHeight))
\tplot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
\tplot
})""" % (pD["x_label"] , pD["y_label"])
    sRL.append(sr)
    
#END OF RSHINY SERVER R CODE



#BEGINNING OF RSHINY UI R CODE

###INITIAL RSHINY UI R CODE
ur = """library(shiny)
library(ggplot2)
library(plotly)
rrdf <- read.table("%s/run_reports/project_only/all.lanes.tsv",header=TRUE,sep="\\t")

shinyUI(fluidPage(
\theaderPanel("%s Run Report Analysis"),
\thr(), 
""" % (data_dir, project)
uRL.append(ur)

###PER PLOT RSHINY UI R CODE

#BAR PLOTS
for plot_key in plots_L:
    pD = plots_D[plot_key]
    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
    
    fail_slider_max_string = ""
    if pD["window_height_type"] == "dynamic":
        if pD["text_to_remove"] == "None":
            fail_slider_max_string = '''max(rrdf$%s)''' % (pD["r_column"])
        else:
            fail_slider_max_string = '''max(as.numeric(gsub("%s","",rrdf$%s))) * 1.10''' % (pD["text_to_remove"] , pD["r_column"])
            
    elif pD["window_height_type"] == "fixed":
        fail_slider_max_string = pD["window_height_max"]
    
    
    
    ur = """\ttitlePanel("%s"),
\tplotlyOutput("%sPlot", height = "80vh"),
\thr(),
\tfluidRow(
\t\tcolumn(3,
\t\t\tsliderInput("%sFailThreshold",
\t\t\t\t"fail threshold:",
\t\t\t\tmin = 0,
\t\t\t\tmax = %s,
\t\t\t\tvalue = 0
\t\t\t)
\t\t),
\t\tcolumn(3,
\t\t\tselectInput("%sGroup", "Group by:",
\t\t\t\tc("None" = "none",
\t\t\t\t"Run" = "Run",
\t\t\t\t"Lane" = "Lane",
\t\t\t\t"Run and Lane" = "Run_Lane",
\t\t\t\t"Barcode" = "Barcode",
\t\t\t\t"Study" = "Study",
\t\t\t\t"Subject" = "Subject",
\t\t\t\t"Sample" = "Sample",
\t\t\t\t"Tissue Type and Origin" = "Tissue_type_origin",
\t\t\t\t"Date" = "Date",
\t\t\t\t"Instrument" = "Instrument",
\t\t\t\t"Increment" = "Increment",
\t\t\t\t"Flowcell" = "Flowcell")
\t\t\t)
\t\t)
\t),
\thr(),""" % (pD["ui_title"] , tab_plot_var , \
                tab_plot_var , fail_slider_max_string,\
                tab_plot_var )
    uRL.append(ur)
    
for xyplot_key in xyplots_L:
    pD = xyplots_D[xyplot_key]
    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
    
    ur = """\ttitlePanel("%s"),
\tplotOutput("%sPlot", height = "80vh"),
\thr(),""" % (pD["ui_title"] , tab_plot_var)
    uRL.append(ur)


toreplace = uRL[-1]
uRL.pop()
uRL.append(toreplace[:-1])

#ENDING RSHINY UI R CODE
ur = """))
"""
uRL.append(ur)

sROut = instance_dir+"/modules/run_reports/run_reports.server.R"
open(sROut,"w").write("\n".join(sRL)+"\n")

uROut = instance_dir+"/modules/run_reports/run_reports.ui.R"
open(uROut,"w").write("\n".join(uRL)+"\n")
