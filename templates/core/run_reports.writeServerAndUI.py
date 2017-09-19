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
    
    #FRONT-END FUNCTION TO RENDER DYNAMIC INTERFACE (COLLAPSABLE OPTIONS PANEL)
    
    #use "window_height_type" key to determine slider maximum for fail threshold
    fail_slider_max_string = ""
    if pD["window_height_type"] == "dynamic":
        if pD["text_to_remove"] == "None":
            fail_slider_max_string = '''max(rrdf$%s)''' % (pD["r_column"])
        else:
            fail_slider_max_string = '''max(as.numeric(gsub("%s","",rrdf$%s))) * 1.10''' % (pD["text_to_remove"] , pD["r_column"])
            
    elif pD["window_height_type"] == "fixed":
        fail_slider_max_string = pD["window_height_max"]
    
    #syntax to render collapsable side bar and also the main plot of interest
    
    sr = """output$%sUI <- renderUI ({
\tif (input$%sShowPanel) {
\t\tsidebarLayout(
\t\t\tsidebarPanel(

\t\t\t\tradioButtons(
\t\t\t\t\t"%sOrientationRadio",
\t\t\t\t\t"Plot Orientation",
\t\t\t\t\tc("horizontal" , "vertical")
\t\t\t\t),

\t\t\t\tradioButtons(
\t\t\t\t\t"%sPlotTypeRadio",
\t\t\t\t\t"Plot Type",
\t\t\t\t\tc("point" , "bar")
\t\t\t\t),

\t\t\t\tradioButtons(
\t\t\t\t\t"%sPlotSize",
\t\t\t\t\t"Plot Size",
\t\t\t\t\tc("full size" , "fit to screen")
\t\t\t\t),

\t\t\t\tsliderInput("%sFailThreshold",
\t\t\t\t\t"Fail Threshold:",
\t\t\t\t\tmin = 0,
\t\t\t\t\tmax = %s,
\t\t\t\t\tvalue = 0
\t\t\t\t),

\t\t\t\tselectInput("%sGroup", "Group by:",
\t\t\t\t\tc("None" = "none",
\t\t\t\t\t"Run" = "Run",
\t\t\t\t\t"Lane" = "Lane",
\t\t\t\t\t"Run and Lane" = "Run_Lane",
\t\t\t\t\t"Barcode" = "Barcode",
\t\t\t\t\t"Study" = "Study",
\t\t\t\t\t"Subject" = "Subject",
\t\t\t\t\t"Sample" = "Sample",
\t\t\t\t\t"Tissue Type and Origin" = "Tissue_type_origin",
\t\t\t\t\t"Date" = "Date",
\t\t\t\t\t"Instrument" = "Instrument",
\t\t\t\t\t"Increment" = "Increment",
\t\t\t\t\t"Flowcell" = "Flowcell"),
\t\t\t\t),

\t\t\t\tselectInput("%sSelectColumn1", "Select for data where:",
\t\t\t\t\tc("None" = "none",
\t\t\t\t\t"Run" = "Run",
\t\t\t\t\t"Lane" = "Lane",
\t\t\t\t\t"Run and Lane" = "Run_Lane",
\t\t\t\t\t"Barcode" = "Barcode",
\t\t\t\t\t"Study" = "Study",
\t\t\t\t\t"Subject" = "Subject",
\t\t\t\t\t"Sample" = "Sample",
\t\t\t\t\t"Tissue Type and Origin" = "Tissue_type_origin",
\t\t\t\t\t"Date" = "Date",
\t\t\t\t\t"Instrument" = "Instrument",
\t\t\t\t\t"Increment" = "Increment",
\t\t\t\t\t"Flowcell" = "Flowcell")
\t\t\t\t),
\t\t\t\tselectInput("%sSelectType1", "",
\t\t\t\t\tc("equals" = "equals",
\t\t\t\t\t"contains" = "contains",
\t\t\t\t\t"does not equal" = "does_not_equal",
\t\t\t\t\t"does not contain" = "does_not_contain")
\t\t\t\t),
\t\t\t\ttextInput("%sSelectValue1", "", "")

\t\t\t),
\t\t\tmainPanel(
\t\t\t\tplotlyOutput("%sPlot" , height="120vh")
\t\t\t)
\t\t)
\t} else {
\t\tplotlyOutput("%sPlot" , height="120vh")
\t}
})
""" % (tab_plot_var , tab_plot_var , tab_plot_var , tab_plot_var , tab_plot_var , tab_plot_var , fail_slider_max_string , tab_plot_var , tab_plot_var , tab_plot_var , tab_plot_var , tab_plot_var , tab_plot_var)
    sRL.append(sr)


    #TEST NEW UI LAYOUT
    """
"""
    
    
    #BACK-END FUNCTION TO GENERATE PLOT
    sr = """output$%sPlot <- renderPlotly({
\tt <- input$%sFailThreshold
""" % (tab_plot_var , tab_plot_var)
    sRL.append(sr)
    
    #IF USER-SPECIFIES SELECTION VIA UI, SELECT FOR ONLY A SUBSET OF DATA
    sr = "\tselect_column1 <- input$%sSelectColumn1\n" % (tab_plot_var) +\
    "\tselect_type1 <- input$%sSelectType1\n" % (tab_plot_var) +\
    "\tselect_value1 <- input$%sSelectValue1\n" % (tab_plot_var) +\
    """\tif (select_column1 != "none") {
\t\tif (select_type1 == "equals") {
\t\t\trrdf <- rrdf[which(rrdf[[select_column1]] == select_value1),]
\t\t} else if (select_type1 == "contains") {
\t\t\trrdf <- rrdf[which(grepl(select_value1 , rrdf[[select_column1]])),]
\t\t} else if (select_type1 == "does_not_equal") {
\t\t\trrdf <- rrdf[which(rrdf[[select_column1]] != select_value1),]
\t\t} else if (select_type1 == "does_not_contain") {
\t\t\trrdf <- rrdf[which(!grepl(select_value1 , rrdf[[select_column1]])),]
\t\t}
\t}"""
    sRL.append(sr)
    
    #GROUP DATA FRAME BY USER-SPECIFIED GROUP
    sr = """\tgroupby <- input$%sGroup
\tif (groupby != "none") {
\t\trrdf <- rrdf[order(rrdf[[groupby]]),]
\t}
""" % (tab_plot_var)
    sRL.append(sr)
    
    
    sr = """\tmyseq <- seq(from=1,to=nrow(rrdf))
\trrdf$record <- myseq"""
    sRL.append(sr)
    
    sr = ""
    if pD["text_to_remove"] == "None":
        sr = '''\td <- data.frame(rrdf$Library, rrdf$record, rrdf$%s)''' % (pD["r_column"])
    else:
        sr = '''\td <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("%s","",rrdf$%s)))''' % (pD["text_to_remove"] , pD["r_column"])
    sRL.append(sr)
    
    sr = """\td$is_pass <- as.factor((d[,3] > t)*1)
\tcolnames(d) <- c("library","record","value","is_pass")
\td <- data.frame(d , rrdf)"""
    sRL.append(sr)
    
    
    
    sr=""
    if pD["window_height_type"] == "dynamic":
        sr = "\twindowHeight <- max(d$value) * 1.10"    
    elif pD["window_height_type"] == "fixed":
        sr = "\twindowHeight <- %s" % (pD["window_height_max"])
    sRL.append(sr)
    
    sr="""\torientation <- input$%sOrientationRadio
\tplot_type <- input$%sPlotTypeRadio

\tplot <- ggplot(d, aes(x=record, y=value))
\tif (plot_type == "bar") {
\t\tplot <- plot + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "\nRun:" , Run , "\nLane:" , Lane , "\nBarcode:",Barcode) ,fill=is_pass))   
\t} else if (plot_type == "point") {
\t\tplot <- plot + geom_point(stat="identity" , aes(text=paste("Library:" , library , "\nRun:" , Run , "\nLane:" , Lane , "\nBarcode:",Barcode) , color=is_pass))   
\t}

\tplot <- plot + labs(x="all libraries", y="%s")
\tplot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
\tplot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(rrdf) , ymin=t , ymax=t), color="red" , linetype="dashed")
\tplot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")
\tunique.groupby <- unique(rrdf[[groupby]])

\tfirst_records <- c()
\tlast_records <- c()
\tmy_cols <- c()
\tmy_cols2 <- c()
\ty1 <- c()
\ty2 <- c()
\ty3 <- c()
\ty4 <- c()
    
\tif (groupby != "none") {
\t\tfor (i in 1:length(unique.groupby)){
\t\t\tunique.group <- unique.groupby[i]
\t\t\tgroup.lines <- rrdf[which(rrdf[[groupby]] == unique.group),]
\t\t\tfirst.record <- head(group.lines , n=1)$record - 0.5
\t\t\tlast.record <- tail(group.lines , n=1)$record + 0.5
\t\t\tcolor <- (i %s 2) + 10
\t\t\tcolor2 <- (i %s 2) + 20
            
\t\t\tfirst_records[i] <- first.record
\t\t\tlast_records[i] <- last.record
\t\t\tmy_cols[i] <- color
\t\t\tmy_cols2[i] <- color2
\t\t\ty1[i] <- windowHeight * 0.98
\t\t\ty2[i] <- windowHeight
\t\t\ty3[i] <- 0.0
\t\t\ty4[i] <- windowHeight
\t\t}
        
\t\tgroup.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
\t\tcolnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(groupby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(groupby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(groupby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")  
\t}

\tplot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
\tplot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
\tplot <- plot + scale_color_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))

\tif (orientation == "horizontal") {
\t\tplot <- plot + scale_x_continuous(trans="reverse")
\t\tplot <- plot + coord_flip()    
\t}

\tggplotly(plot)
})""" % (tab_plot_var , tab_plot_var , pD["y_label"] , "%"*2 , "%"*2)
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

shinyUI(
\tnavbarPage(
\t\t"%s Run Reports",
\t\tnavbarMenu("Plots",""" % (data_dir , project)
uRL.append(ur)

###PER PLOT RSHINY UI R CODE

#BAR PLOTS
for plot_key in plots_L:
    pD = plots_D[plot_key]
    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
    
    ur = """\t\t\ttabPanel("%s",
\t\t\t\tfluidPage(
\t\t\t\t\tbsButton("%sShowPanel","show/hide sidebar",type="toggle",value=TRUE),
\t\t\t\t\tuiOutput('%sUI')
\t\t\t\t)
\t\t\t),""" % (pD["ui_title"] , tab_plot_var , tab_plot_var)
    uRL.append(ur)
toreplace = uRL[-1]
uRL.pop()
uRL.append(toreplace[:-1])
        
#for xyplot_key in xyplots_L:
#    pD = xyplots_D[xyplot_key]
#    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
    
#    ur = """\ttitlePanel("%s"),
#\tplotOutput("%sPlot", height = "80vh"),
#\thr(),""" % (pD["ui_title"] , tab_plot_var)
#    uRL.append(ur)

#toreplace = uRL[-1]
#uRL.pop()
#uRL.append(toreplace[:-1])

#ENDING RSHINY UI R CODE
ur = """\t\t)
\t)
)"""
uRL.append(ur)

sROut = instance_dir+"/modules/run_reports/run_reports.server.R"
open(sROut,"w").write("\n".join(sRL)+"\n")

uROut = instance_dir+"/modules/run_reports/run_reports.ui.R"
open(uROut,"w").write("\n".join(uRL)+"\n")
