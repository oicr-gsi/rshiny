import yaml
import os

with open('config.yaml') as f:
    # use safe_load instead load
    dataMap = yaml.safe_load(f)
    
    cmd = "python %s/deploy.py %s/config.yaml" % (dataMap["app-instance"]["app_dir"] , dataMap["app-instance"]["instance_dir"])
    os.system(cmd)
