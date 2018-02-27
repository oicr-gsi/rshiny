import yaml
import sys
import os

config_yaml = sys.argv[1]
dataMap = {}
with open(config_yaml) as f:
    # use safe_load instead load
    dataMap = yaml.safe_load(f)
    
app_dir = dataMap["app-instance"]["app_dir"]
project = dataMap["app-instance"]["project"]
    
data_dir = dataMap["app-instance"]["data_dir"]
instance_dir = dataMap["app-instance"]["instance_dir"]

r_template_dir = app_dir+"/r.templates/run_report"
r_template_D = {
    "server_intro" : r_template_dir+"/server.intro.R",
    "barplot_function_render_plot" : r_template_dir+"/barplot.function.render.plot.R",
    "barplot_function_render_ui" : r_template_dir+"/barplot.function.render.ui.R",
    "ui_intro" : r_template_dir+"/ui.intro.R",
    "ui_outtro" : r_template_dir+"/ui.outtro.R",
    "ui_barplot" : r_template_dir+"/ui.barplot.R"
}

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
serverR = []
uiR = []


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
s = open(r_template_D["server_intro"]).read().\
                                                replace(".DATA.DIR.",data_dir)
serverR.append(s)

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
    s = open(r_template_D["barplot_function_render_ui"]).read().\
                                                                replace(".TAB.PLOT.VAR.",tab_plot_var).\
                                                                replace(".FAIL.SLIDER.MAX.STRING.",fail_slider_max_string)
    serverR.append(s)
    
    #BACK-END FUNCTION TO GENERATE PLOT
    
    #get syntax for properly formatted y-variable
    y_value_var = ""
    if pD["text_to_remove"] == "None":
        y_value_var = '''rrdf$%s''' % (pD["r_column"])
    else:
        y_value_var = '''as.numeric(gsub("%s","",rrdf$%s))''' % (pD["text_to_remove"] , pD["r_column"])
    
    #get syntax for window height
    window_height_string=""
    if pD["window_height_type"] == "dynamic":
        window_height_string = "max(d$value) * 1.10"
    elif pD["window_height_type"] == "fixed":
        window_height_string = pD["window_height_max"]
        
    #get syntax for y axis label
    y_label = pD["y_label"]
    
    s = open(r_template_D["barplot_function_render_plot"]).read().\
                                                                replace(".TAB.PLOT.VAR.",tab_plot_var).\
                                                                replace(".Y.VALUE.VAR.",y_value_var).\
                                                                replace(".WINDOW.HEIGHT.STRING.",window_height_string).\
                                                                replace(".Y.LABEL.",y_label)
    serverR.append(s)

#XY PLOTS
#for xyplot_key in xyplots_L:
#    pD = xyplots_D[xyplot_key]
#    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
#
#    sr = """output$%sPlot <- renderPlot({
# % (tab_plot_var)
#    sRL.append(sr)
#    
#    sr = "\td <- data.frame(rrdf$Library, "
#    
#    if pD["x_text_to_remove"] == "None":
#
#        sr = sr + '''rrdf$%s, ''' % (pD["x_r_column"])
#    else:
#        sr = sr + '''as.numeric(gsub("%s","",rrdf$%s)), ''' % (pD["x_text_to_remove"] , pD["x_r_column"])
#
#        
#    if pD["y_text_to_remove"] == "None":
#
#
#        sr = sr + '''rrdf$%s)''' % (pD["y_r_column"])
#    else:
#        sr = sr + '''as.numeric(gsub("%s","",rrdf$%s)))''' % (pD["y_text_to_remove"] , pD["y_r_column"])
#
#    sRL.append(sr)
#    
#    sr = """\tcolnames(d) <- c("library","my_x","my_y")"""
#    sRL.append(sr)
#    
#    
#    sr=""
#    if pD["window_width_type"] == "dynamic":
#        sr = "\twindowWidth <- max(d$my_x) * 1.10"    
#    elif pD["window_width_type"] == "fixed":
#        sr = "\twindowWidth <- %s" % (pD["window_width_max"])
#    sRL.append(sr)
#    
#    sr=""
#    if pD["window_height_type"] == "dynamic":
#        sr = "\twindowHeight <- max(d$my_y) * 1.10"    
#    elif pD["window_height_type"] == "fixed":
#        sr = "\twindowHeight <- %s" % (pD["window_height_max"])
#    sRL.append(sr)
#    
#    sr="""\tplot <- ggplot(d, aes(x=my_x, y=my_y)) + geom_point() + labs(x="%s", y="%s") + scale_x_continuous(limits=c(0.0,windowWidth)) + scale_y_continuous(limits=c(0.0,windowHeight))
#\tplot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
#\tplot
#})""" % (pD["x_label"] , pD["y_label"])
#    sRL.append(sr)
    
#END OF RSHINY SERVER R CODE

#BEGINNING OF RSHINY UI R CODE

###INITIAL RSHINY UI R CODE
s = open(r_template_D["ui_intro"]).read().\
                                            replace(".DATA.DIR.",data_dir).\
                                            replace(".PROJECT.",project)
uiR.append(s)

###PER PLOT RSHINY UI R CODE

#BAR PLOTS
for plot_key in plots_L:
    pD = plots_D[plot_key]
    tab_plot_var = tab_var + "_" + pD["rvar_prefix"]
    
    #get syntax for y axis label
    y_label = pD["y_label"]
    
    s = open(r_template_D["ui_barplot"]).read().\
                                            replace(".TAB.PLOT.VAR.",tab_plot_var).\
                                            replace(".Y.LABEL.",y_label)
    uiR.append(s)
    
toreplace = uiR[-1]
uiR.pop()
uiR.append(toreplace[:-2]+"\n")
        
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
s = open(r_template_D["ui_outtro"]).read()
uiR.append(s)


sROut = instance_dir+"/modules/run_reports/run_reports.server.R"
open(sROut,"w").write("\n".join(serverR)+"\n")

uROut = instance_dir+"/modules/run_reports/run_reports.ui.R"
open(uROut,"w").write("\n".join(uiR)+"\n")
