import pandas as pd
import os
#import urllib.request
from io import BytesIO
import requests, zipfile


"""
Input_Data <- 'https://physionet.org/static/published-projects/mimic-iv-demo/mimic-iv-clinical-database-demo-1.0.zip';
  dir.create('data',showWarnings = FALSE);
  Zipped_Data <- file.path("data",'tempdata.zip'); # set file path of data\tempdata.zip
  if(!file.exists(Zipped_Data)) download.file(Input_Data,destfile = Zipped_Data);
  Unzipped_Data <- unzip(Zipped_Data,exdir = 'data') %>% grep('gz$',.,val=T);
"""

data_file='data_py'

Input_Data = 'https://physionet.org/static/published-projects/mimic-iv-demo/mimic-iv-clinical-database-demo-1.0.zip';

if not os.path.exists(data_file) : 
  os.mkdir(data_file) 

if not os.path.exists(os.path.join(data_file,'mimic-iv-clinical-database-demo-1.0')):
  Zipped_Data =BytesIO(requests.get(Input_Data,stream=True).content)
  zipfile.ZipFile(Zipped_Data).extractall(data_file)

mimic = {filename.replace ('.csv.gz',''): pd.read_csv(dirpath + "/" + filename) 
              for dirpath, dirnames, filenames in os.walk(data_file) 
              for filename in filenames if filename.endswith('.gz')}

mimic.keys()
mimic['patients']
