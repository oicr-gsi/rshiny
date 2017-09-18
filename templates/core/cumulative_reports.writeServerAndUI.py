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
    "percent-excluded-reads",
    "read-length",
    "percent-on-target",
    "mean-coverage",
    "percent-coverage-at-8x",
    "minimum-coverage-for-90-percent-of-target"
]

#sRL: code for server.R app
#uRL: code for ui.R app
sRL = []
uRL = []

#DICTIONARY containing the parameters for the various barplots that will be produced through ggplot2
plots_D = {
    "total-reads" : {
        "rvar_prefix" : "AggregateReads",
        "r_column" : "Aggregate.PF.Reads",
        "text_to_remove" : ",",
        "y_label" : "aggregate reads (pass filter)",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Aggregate Reads (Pass filter)"
    },
    
    "percent-excluded-reads" : {
        "rvar_prefix" : "percentAggregateExcludedReads",
        "r_column" : "Aggregate.PF.Excluded.Reads..",
        "text_to_remove" : "%",
        "y_label" : "percent of aggregate reads excluded from alignment",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent Aggregate Excluded Reads"
    },
      
    "read-length" : {
        "rvar_prefix" : "averageReadLength",
        "r_column" : "Average.Read.Length",
        "text_to_remove" : "None",
        "y_label" : "average read length",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Average Read Length"
    },
        
    "percent-on-target" : {
        "rvar_prefix" : "percentOnt",
        "r_column" : "Merged.Mapped.Reads.On.Target..",
        "text_to_remove" : "%",
        "y_label" : "percent of mapped reads on target",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent On Target"
    },
        
    "mean-coverage" : {
        "rvar_prefix" : "meanCoverage",
        "r_column" : "Average.Coverage",
        "text_to_remove" : "None",
        "y_label" : "mean coverage",
        
        "window_height_type" : "dynamic",
        "window_height_max" : "na",
        
        "ui_title" : "Mean Coverage"
    },
        
    "percent-coverage-at-8x" : {
        "rvar_prefix" : "percentCoverageEightX",
        "r_column" : "Base.Coverage.At.8x",
        "text_to_remove" : "%",
        "y_label" : "percent of target bases covered at 8x or higher",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Percent of Target Covered at 8x or Higher"
    },
        
    "minimum-coverage-for-90-percent-of-target" : {
        "rvar_prefix" : "minCoverageForNinetyPercentOfTarget",
        "r_column" : "X90..Covered.At..x.",
        "text_to_remove" : "x",
        "y_label" : "minimum level of coverage for 90% of target bases",
        
        "window_height_type" : "fixed",
        "window_height_max" : "100.0",
        
        "ui_title" : "Minimum Coverage for 90% of Target"
    }
        
}

tab_var = "cumlativeReports" #variable indicating the tab, so that back-end variable names do not overlap with those from other tabs

###BEGINNING OF SERVER R CODE

###INITIAL RSHINY SERVER R CODE
sr = """library(shiny)
library(ggplot2)
library(plotly)

gg_color_hue <- function(n) {
\thues = seq(15, 375, length = n + 1)
\thcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("%s/cumulative_reports/cumulative.report.formatted.tsv",header=TRUE,sep="\\t")
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
            fail_slider_max_string = '''max(df$%s)''' % (pD["r_column"])
        else:
            fail_slider_max_string = '''max(as.numeric(gsub("%s","",df$%s))) * 1.10''' % (pD["text_to_remove"] , pD["r_column"])
            
    elif pD["window_height_type"] == "fixed":
        fail_slider_max_string = pD["window_height_max"]
    
    #syntax to render collapsable side bar and also the main plot of interest
    sr = """output$%sUI <- renderUI ({
\tif (input$%sShowPanel) {
\t\tsidebarLayout(
\t\t\tsidebarPanel(
\t\t\t\tsliderInput("%sFailThreshold",
\t\t\t\t\t"fail threshold:",
\t\t\t\t\tmin = 0,
\t\t\t\t\tmax = %s,
\t\t\t\t\tvalue = 0
\t\t\t\t),
\t\t\t\tselectInput("%sGroup", "Group by:",
\t\t\t\t\tc("None" = "none",
\t\t\t\t\t"Last Modified" = "Last.Modified",
\t\t\t\t\t"Sample" = "Sample",
\t\t\t\t\t"Study" = "Study",
\t\t\t\t\t"Subject" = "Subject",
\t\t\t\t\t"Sample without GroupID" = "SampleNoGID",
\t\t\t\t\t"Tissue Type and Origin" = "Tissue_type_origin",
\t\t\t\t\t"Tissue Type, Origin, and GroupID" = "Tissue_type_origin_groupid")
\t\t\t\t)
\t\t\t),
\t\t\tmainPanel(
\t\t\t\tplotlyOutput("%sPlot" , height="120vh")
\t\t\t)
\t\t)
\t} else {
\t\tplotlyOutput("%sPlot" , height="120vh")
\t}
})
""" % (tab_plot_var , tab_plot_var , tab_plot_var , fail_slider_max_string , tab_plot_var , tab_plot_var , tab_plot_var)
    sRL.append(sr)
    
    
    #BACK-END FUNCTION TO GENERATE PLOT
    sr = """output$%sPlot <- renderPlotly({
\tmyseq <- seq(from=1,to=nrow(df))
\tt <- input$%sFailThreshold
""" % (tab_plot_var , tab_plot_var)
    sRL.append(sr)
    
    
    sr = """\tsortby <- input$%sGroup
\tif (sortby != "none") {
\t\tdf <- df[order(df[[sortby]]),]
\t}
""" % (tab_plot_var)
    sRL.append(sr)
    
    sr = "\tdf$record <- myseq"
    sRL.append(sr)
    
    sr = ""
    if pD["text_to_remove"] == "None":
        sr = '''\td <- data.frame(df$Sample, df$record, df$%s)''' % (pD["r_column"])
    else:
        sr = '''\td <- data.frame(df$Sample , df$record, as.numeric(gsub("%s","",df$%s)))''' % (pD["text_to_remove"] , pD["r_column"])
    sRL.append(sr)
    
    sr = """\td$group <- as.factor((d[,3] > t)*1)
\tcolnames(d) <- c("sample","record","value","threshold")
\td <- data.frame(d , df)"""
    sRL.append(sr)
    
    
    
    sr=""
    if pD["window_height_type"] == "dynamic":
        sr = "\twindowHeight <- max(d$value) * 1.10"    
    elif pD["window_height_type"] == "fixed":
        sr = "\twindowHeight <- %s" % (pD["window_height_max"])
    sRL.append(sr)
    
    sr="""\tplot <- ggplot(d, aes(x=record, y=value))
\tplot <- plot + geom_bar(stat="identity" , aes(text=paste("Sample:" , Sample , "\nRuns:Libraries:Lanes:" , Runs.Libraries.Lanes , "\nLast Modified:", Last.Modified) ,fill=threshold))
\tplot <- plot + labs(x="all libraries", y="%s")
\tplot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
\tplot <- plot + scale_x_continuous(trans="reverse")
\tplot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(df) , ymin=t , ymax=t), color="red" , linetype="dashed")
\tplot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")  
\tunique.groupby <- unique(df[[sortby]])

\tfirst_records <- c()
\tlast_records <- c()
\tmy_cols <- c()
\tmy_cols2 <- c()
\ty1 <- c()
\ty2 <- c()
\ty3 <- c()
\ty4 <- c()
    
\tif (sortby != "none") {
\t\tfor (i in 1:length(unique.groupby)){
\t\t\tunique.group <- unique.groupby[i]
\t\t\tgroup.lines <- df[which(df[[sortby]] == unique.group),]
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
\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
\t\tplot <- plot + geom_rect(data=group.df , aes(text=paste(sortby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
\t}

\tplot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
\tplot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
\tplot <- plot + coord_flip()
\tggplotly(plot)
})""" % (pD["y_label"] , "%"*2 , "%"*2)
    sRL.append(sr)

#END OF RSHINY SERVER R CODE



#BEGINNING OF RSHINY UI R CODE

###INITIAL RSHINY UI R CODE
ur = """library(shiny)
library(ggplot2)
library(plotly)
df <- read.table("%s/cumulative_reports/cumulative.report.formatted.tsv",header=TRUE,sep="\\t")

shinyUI(
\tnavbarPage(
\t\t"%s Cumulative Reports",
\t\tnavbarMenu("Plots",""" % (data_dir, project)
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

#ENDING RSHINY UI R CODE
ur = """\t\t)
\t)
)"""
uRL.append(ur)

sROut = instance_dir+"/modules/cumulative_reports/cumulative_reports.server.R"
open(sROut,"w").write("\n".join(sRL)+"\n")

uROut = instance_dir+"/modules/cumulative_reports/cumulative_reports.ui.R"
open(uROut,"w").write("\n".join(uRL)+"\n")
