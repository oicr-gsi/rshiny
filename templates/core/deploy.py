import yaml
import sys
import os

config_yaml = sys.argv[1]

with open('config.yaml') as f:
    # use safe_load instead load
    dataMap = yaml.safe_load(f)
    
    app_dir = dataMap["app-instance"]["app_dir"]
    project = dataMap["app-instance"]["project"]
    
    data_dir = dataMap["app-instance"]["data_dir"]
    instance_dir = dataMap["app-instance"]["instance_dir"]
    shiny_dir = dataMap["app-instance"]["shiny_dir"]
    
    #step 1: create all sub-directories
    subdirs_L = ["www",\
                 "modules",\
                 "modules/run_reports",\
                 "modules/cumulative_reports"]
    
    for subdir in subdirs_L:
        if os.path.exists(instance_dir+"/"+subdir) == False:
            os.mkdir(instance_dir+"/"+subdir)
    
    #step 2: write instantiated app server.R and ui.R to instance directory
    app_server_file = app_dir+"/server.R"
    app_ui_file = app_dir+"/ui.R"
    
    instance_server_file = instance_dir+"/server.R"
    instance_ui_file = instance_dir+"/ui.R"
    
    open(instance_server_file,"w").write(\
                                         open(app_server_file,"r").read().replace("VAR_INSTANCE_DIR",instance_dir)
                                        )
    
    open(instance_ui_file,"w").write(\
                                         open(app_ui_file,"r").read().replace("VAR_INSTANCE_DIR",instance_dir).replace("VAR_PROJECT",project)
                                    )
    
    #step 3: run run_reports.writeServerAndUI.py script to write instance-specific run reports server and ui files
    cmd = "python %s/run_reports.writeServerAndUI.py %s" % (app_dir,config_yaml)
    os.system(cmd)
    
    #step 4: run cumulative_reports.writeServerAndUI.py script to write instance-specific cumulative reports server and ui files
    cmd = "python %s/cumulative_reports.writeServerAndUI.py %s" % (app_dir,config_yaml)
    os.system(cmd)
    
    #step 5: create styles.css file (will add to this as tool becomes more sophisticated)
    cmd = "touch %s/www/styles.css" % (instance_dir)
    os.system(cmd)
    
    #step 6: link instance dir to shiny apps dir
    cmd = "sudo ln -s %s %s" % (instance_dir,shiny_dir)
    os.system(cmd)
    
    

