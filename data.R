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
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);

library(ggplot2); # visualisation
library(GGally);
library(rio);# simple command for importing and exporting data
library(pander); # format tables
library(printr); # automatically invoke pander when tables are detected
library(broom); # standardized, enhanced views of various objects
library(dplyr); # table manipulation
library(fs);    # file system operations
library(purrr) # package contains map2
library(tidyr) # package contains unnest


options(max.print=42);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);

lengthunique<-function(xx){
  unique(xx) %>% length()
}

uniquevalues<-function(xx){
  unique(xx) %>% sort() %>% paste(collapse=":")
    }


democolumns<-c('subject_id','insurance','marital_status','ethnicity')

if(!file.exists('data.R.rdata')){
  # Import the data
  Input_Data <- 'https://physionet.org/static/published-projects/mimic-iv-demo/mimic-iv-clinical-database-demo-1.0.zip';
  dir.create('data',showWarnings = FALSE);
  Zipped_Data <- file.path("data",'tempdata.zip');
  download.file(Input_Data,destfile = Zipped_Data);
  Unzipped_Data <- unzip(Zipped_Data,exdir = 'data') %>% grep('gz$',.,val=T);
  Table_Names <- path_ext_remove(Unzipped_Data) %>% path_ext_remove() %>% basename;#extract basename from the unzipped files, removes extensions (gz and csv)
  for(ii in seq_along(Unzipped_Data)) assign(Table_Names[ii],import(Unzipped_Data[ii],format='csv'));
  #seq_along creates a sequence of the same variables used for indexing, importing an unzipped data, assigned to Table_Names
  #mapply(function(aa,bb) assign(aa,import(bb,format='csv'),inherits = T),Table_Names,Unzipped_Data)
  save(list=Table_Names,file='data.R.rdata');
  message("data downloaded")
} else{
message("data already present")
  load("data.R.rdata")
}

sum(!is.na(admissions$deathtime)) # count how many cells in deathtime column is missing
admissions[,democolumns] %>% unique() %>% nrow()



sapply(admissions[,democolumns],lengthunique) # apply a function(lengthunique) to all democolumns in admissions dataframe
sapply(admissions[,democolumns],function(xx) unique(xx) %>% length())

summarise(admissions[,democolumns]
          ,subject_id=lengthunique(subject_id)
          ,insurance=lengthunique(insurance)) # language column has been removed from democolumns already

summarise(admissions[,democolumns]
          ,across(any_of(democolumns),lengthunique))


group_by(admissions,subject_id)%>% summarise(across(any_of(democolumns),lengthunique))

#Start a new section
#' # Demographic Table
#'

demographics<-group_by(admissions,subject_id) %>%
  summarise(across(any_of(democolumns), uniquevalues),
          deceased=any(!is.na(deathtime)),
          deathtime=max(deathtime, na.rm = TRUE)) %>%
  mutate(ethnicity=gsub("UNKNOWN;","",ethnicity)) %>%
  mutate(ethnicity=gsub("UNABLE TO OBTAIN","UNKNOWN",ethnicity)) %>%
  left_join(patients[,1:3])


demographics$deathtime %>% class()
demographics[is.infinite(demographics$deathtime),"deathtime"]=NA

named_outputevents<-left_join(outputevents,d_items,by=c(itemid='itemid'))
named_labevents<-left_join(labevents,d_labitems)
named_chartevents<-left_join(chartevents,d_items)
named_diagnoses<-left_join(diagnoses_icd,d_icd_diagnoses)

# A list of variable (labs/Glucose, A1c, hypoglycemia/diagnosis, death, icu stay, length of icu stay)
#select(admissions,subject_id,hadm_id,admittime,dischtime)

# map2(admittime,dischtime,function(xx,yy) {seq(trunc(xx,u="days"),yy,by="day")})
# created a list

adm_Dates<-transmute(admissions,hadm_id=hadm_id,subject_id=subject_id,
                     date=map2(admittime,dischtime,function(xx,yy)
                       {seq(trunc(xx,u="days"),yy,by="day")})) %>% unnest
# create a scaffold of admission dates

#adm_table = admissions %>% transmute( hadm_id = hadm_id, subject_id = subject_id,
los = ceiling(as.numeric(dischtime - admittime) / 24),
date = purrr::map2(admittime,dischtime, function(xx,yy) seq(trunc(xx,units = 'days'),yy, by = 'day'))


#Homework (9/20/23): create a table with the above scaffold, add an additional column of stay_id in icustays, NA for non-icu stay days, expand on intime and outtime

# another example: by=c(itemid='itemid',date='date') if each entry is indexed by both itemid and date
# Dataexplore::create_report()
# explore::exlore_shiny()

