#'---
#' title: "Data Extraction"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Import data from an online source for use by the remaining scripts in this
#'  | project.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
upload_to_google <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);
refresh <- 0;

authemail <- 'topystone@gmail.com'
projectid <- 'inspiring-tower-401719'
datasetid <- 'Class_Test_Dataset'

# If you have a config_local.R, you can use it to override the default values above
if(file.exists('config_local.R')) source('config_local.R');

library(ggplot2); # visualization
library(GGally);
library(rio);# simple command for importing and exporting data
library(pander); # format tables
library(printr); # automatically invoke pander when tables are detected
library(broom); # standardized, enhanced views of various objects
library(dplyr); # table manipulation
library(fs);    # file system operations
library(purrr); # package contains map2
library(tidyr); # package contains unnest
library(stringr); #string operations
library(googleAuthR); # interacting with Google BigQuery
library(bigQueryR);
library(DataExplorer);
library(explore);

options(max.print=42);
options(datatable.na.strings=c('NA','NULL','')); #defines what is treated as missing/NA
options(datatable.integer64='numeric');
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);


# function of length of each unique variables
lengthunique <- function(xx){
  unique(xx) %>% length()
}

# function of paste each unique values in ascending order and separated by ":" inbetween
uniquevalues <- function(xx){
  unique(xx) %>% sort() %>% paste(collapse=":")
  }

# you can save a pipe line as a function by starting it with '.'
count_by_freq <- . %>% summarise(n=n(),patients=lengthunique(subject_id)) %>% arrange(desc(n));

democolumns<-c('subject_id','insurance','marital_status','ethnicity')

# get a vector of all the objects created by this script so far by running the ls()
# command with no arguments
Starting_Names<-ls()

# Only take the time to obtain and extract data if data.R.rdata doesn't already
# exist
if(!file.exists('data.R.rdata')|refresh){
  # Import the data
  Input_Data <- 'https://physionet.org/static/published-projects/mimic-iv-demo/mimic-iv-clinical-database-demo-1.0.zip';
  dir.create('data',showWarnings = FALSE);
  # set file path of data\tempdata.zip
  Zipped_Data <- file.path("data",'tempdata.zip');
  # Only download the zipped file if THAT doesn't exist
  if(!file.exists(Zipped_Data)) download.file(Input_Data,destfile = Zipped_Data);
  Unzipped_Data <- unzip(Zipped_Data,exdir = 'data') %>% grep('gz$',.,val=T);
  #extract basename from the unzipped files, removes extensions (gz and csv)
  Table_Names <- path_ext_remove(Unzipped_Data) %>% path_ext_remove() %>% basename;
  # importing, unzipping, and assigning each file to the corresponding item in Table_Names
  # seq_along creates a sequence of consecutive integers used for iterating through the table names and files
  for(ii in seq_along(Unzipped_Data)) {
    assign(Table_Names[ii]
           ,import(Unzipped_Data[ii],format='csv') %>%
             # If a function's name is given directly, it's assumed that the first
             # argument is going to be the name of the column. If you need it to
             # be something other than the default first argument use this syntax:
             # ~as.Date(.x) where .x represents the current column name
             mutate(across(where(~is(.x, "IDate")),as.Date))
           )};
  # usage note: mapply(function(aa,bb) assign(aa,import(bb,format='csv'),inherits = T),Table_Names,Unzipped_Data)
  save(list=c(Table_Names,'Table_Names'),file='data.R.rdata');
  message("data downloaded")
} else{
message("data already present")
  load("data.R.rdata")
  }

sum(!is.na(admissions$deathtime))
# total number of non-missing values in deathtime column

admissions[,democolumns] %>% unique() %>% nrow()
# count numbers of unique rows in democolumns


#' # Some data exploration examples, no permanent changes made in this section
sapply(admissions[,democolumns],lengthunique)
# apply a custom function (lengthunique) to all democolumns in the admissions dataframe

sapply(admissions[,democolumns],function(xx) unique(xx) %>% length())
# alternative code, which perform the same function as the one above

summarise(admissions[,democolumns]
          ,subject_id=lengthunique(subject_id)
          ,insurance=lengthunique(insurance))
# language column has been removed from democolumns already

summarise(admissions[,democolumns]
          ,across(any_of(democolumns),lengthunique))
# apply lengthunique function across multiple columns


group_by(admissions,subject_id)%>% summarise(across(any_of(democolumns),lengthunique)) %>% head()

# Start a new section
#' # Demographic Table
#'

demographics<-group_by(admissions,subject_id) %>%
  summarise(across(any_of(democolumns), uniquevalues),
          deceased=any(!is.na(deathtime)),
          deathtime=max(deathtime, na.rm = TRUE)) %>%
# na.rm: a logical indicating whether missing values should be removed
# no non-missing arguments to max; returning -Inf
  mutate(ethnicity=gsub("UNABLE TO OBTAIN","UNKNOWN",ethnicity)) %>%
  mutate(ethnicity=gsub("UNKNOWN;","",ethnicity)) %>%
  left_join(patients[,1:3])


demographics$deathtime %>% class()
demographics[is.infinite(demographics$deathtime),"deathtime"]=NA

named_outputevents<-left_join(outputevents,d_items,by=c(itemid='itemid'))
named_labevents <- left_join(labevents,d_labitems)
named_chartevents <- left_join(chartevents, d_items)
named_diagnoses <- left_join(diagnoses_icd, d_icd_diagnoses)


# A list of variable (labs/Glucose, A1c, hypoglycemia/diagnosis, death, icu stay, length of icu stay)
# select(admissions,subject_id,hadm_id,admittime,dischtime)

# map2(admittime,dischtime,function(xx,yy) {seq(trunc(xx,u="days"),yy,by="day")})
# created a list

# create a scaffold of admission dates
adm_Dates <- transmute(admissions,hadm_id=hadm_id,subject_id=subject_id,
                     los=ceiling(as.numeric(dischtime - admittime)) / 24,
                     date=map2(admittime,dischtime,function(xx,yy)
                       {seq(trunc(xx,units = "days"),yy,by="day")})) %>% unnest()


# icu stay dates

icu_Dates <- icustays %>% transmute(hadm_id, subject_id, stay_id,
                                   ICUlos=los,
                                   ICUlos_revised = ceiling(as.numeric(outtime - intime) / 1440),
                                   ICU_date = purrr::map2(intime,outtime,
                       function(xx,yy) seq(trunc(xx,units = 'days'),yy, by = 'day'))) %>%
  tidyr::unnest(ICU_date) %>%
  mutate(icu_date=as.Date(ICU_date)) %>%
  group_by(hadm_id,subject_id,ICU_date) %>%
# summarise(ICUlos=paste(ICUlos,collapse = "|"),stay_id=paste(stay_id,collapse ="|"))
summarise(ICUlos = list(ICUlos),stay_id = list(stay_id))
# now the columns ICUlos and stay_id are lists, not numbers such as NA

htn_adm<-named_diagnoses %>% subset(str_detect(tolower(long_title),'hypertens')) %>% pull(hadm_id) %>% unique()

hypoG_adm <- MainData_icd<-c("E11649","E161","E162","E160","E15","E13141") %>% paste(.,collapse = "|") %>%
  {subset(named_diagnoses,grepl(.,icd_code))} %>%
  pull(hadm_id) %>% unique()
# The subset function is used to select rows from the named_diagnoses dataset
# where the grepl function returned TRUE.
# This effectively filters the dataset to include only rows with ICD-10 codes
# matching the specified codes in the combined string.

# Combined admissions and ICU_Dates
MainData<-left_join(adm_Dates,icu_Dates, by=c("hadm_id"="hadm_id","subject_id"="subject_id","date"="ICU_date")) %>%
  mutate(hypertension=hadm_id %in% htn_adm,
         hypoglycemia=hadm_id %in% hypoG_adm)

named_labevents %>% group_by(category,fluid,loinc_code,label) %>%
  summarize(n=n(),patients=lengthunique(subject_id)) %>%
  arrange(desc(n))

pH_table <- named_labevents %>% mutate(charttime=as.Date(charttime)) %>%
  filter(itemid==50820) %>%
  group_by(subject_id,charttime) %>%
  summarise(pH=min(valuenum),pH_flag=any(flag=='abnormal')) %>%
  arrange(desc(pH))



# named variables
VS_abbrev<-c(mSBP='Non Invasive Blood Pressure Systolic Left',aSBP='Arterial Blood Pressure systolic',HR='Heart Rate')

analytic_event<-named_chartevents %>% mutate(charttime=as.Date(charttime)) %>%
  group_by(label,subject_id,charttime) %>%
  filter(label %in% VS_abbrev) %>%
           summarise(Median_Value = median(valuenum,na.rm=TRUE)) %>%
  pivot_wider(values_from = Median_Value,names_from = label) %>%
  rename(any_of(VS_abbrev))

MainData<-MainData %>%
  left_join(pH_table, by=c('subject_id','date'='charttime')) %>%
  mutate(pH=coalesce(pH, 7.4)) %>%
  left_join(analytic_event, by = c('subject_id','date' = 'charttime')) %>%
  left_join(demographics)
# adding missing value of pH, assign it to 7.4



#Homework (9/27/23): left join our selected ICD-10 codes to be "yes or no hypoglycemia"

forcats::fct_count(named_diagnoses$icd_code, sort=T)
named_diagnoses %>% group_by(icd_code,long_title) %>%
  summarise(n=n(),patients=lengthunique(subject_id)) %>% arrange(desc(n))

named_diagnoses %>% subset(str_detect(long_title,'hypertens')) %>% select(icd_code) %>%  unique()
# str_detect is not case sensitive
# select: data fram
# pull: TRUE or FALSE

htn_adm<-named_diagnoses %>% subset(str_detect(tolower(long_title),'hypertens')) %>% select(hadm_id) %>%  unique()

# any data frame which contains stay_id, what are the column names
sapply(.GlobalEnv, is.data.frame) %>% .[.] %>% names(.) %>%
  sapply(.,function(xx) get(xx) %>% colnames() %>%
           grepl('stay_id',.) %>% any()) %>% .[.] %>%
  names() %>% sapply(.,function(xx) get(xx) %>% colnames())

# CODES IN PROGRESS
# icu_Dates<-transmute(icustays,hamd_id,subject_id,stay_id,los = ceiling(as.numeric(outtime - intime)/24), date=map2(intime,outtime,function(xx,yy)
# seq(trunc(xx,units = 'days'),yy,by='day')) %>% unnest

# adm_table = admissions %>% transmute(hadm_id = hadm_id, subject_id = subject_id,los = ceiling(as.numeric(dischtime - admittime) / 24) date = purrr::map2(admittime,dischtime, function(xx,yy) seq(trunc(xx,units = 'days'),yy, by = 'day'))


# another example: by=c(itemid='itemid',date='date') if each entry is indexed by both itemid and date
# Dataexplore::create_report()
# explore::exlore_shiny()

# .GlobalEnv
# sapply(.GlobalEnv, is.data.frame) %>% .[.] %>% names(.) %>% sapply(.,function(xx) get(xx))
# sapply(.GlobalEnv, is.data.frame) %>% .[.] %>% names(.) %>% sapply(.,function(xx) get(xx) %>% colnames() %>% grepl('stay_id',.) %>% any())
# sapply(.GlobalEnv, is.data.frame) %>% .[.] %>% names(.) %>% sapply(.,function(xx) get(xx) %>% colnames() %>% str_detect(.,'stay_id') %>% any())

# icu_Dates %>% group_by(subject_id, ICU_date) %>%
# summarise(number=n(),number_stays=length(unique(stay_id))) %>%
# subset(number>1) %>% pull(subject_id) %>% {subset(icustays,subject_id %in% .)}

# icu_Dates %>% group_by(subject_id, ICU_date) %>%
# summarise(n=n(),number_stays=length(unique(stay_id))) %>%
#  subset(number>1) %>% pull(subject_id) %>% {subset(icustays,subject_id %in% .)}

# named_diagnoses[grep("hypogly",named_diagnoses$long_title,ignore.case = TRUE),]
# grep(c("E1164*","E15","E16"),named_diagnoses$icd_cod)


# SQL (BigQuery)

if(upload_to_google && file.exists('Service_Account_SQL.json')){
  gar_cache_empty()
  # Service_Account_SQL.json is a file that's supposed to exist on your own
  # local computer. If you ever need to download a new copy or make an OAuth
  # credentials file for a different project, here are some instructions to
  # help you: https://gargle.r-lib.org/articles/get-api-credentials.html
  gar_set_client("Service_Account_SQL.json")
  bqr_auth(email=authemail)
  lapply(Table_Names,function(xx){message(xx);bqr_upload_data(projectid,datasetid,xx,get(xx))})
  # can use lapply or for loop
}

#export(icu_Dates,'icu_Dates.csv')
