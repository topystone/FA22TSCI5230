-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/*WITH Recursive xx as(
  SELECT 1 MyNumber
  UNION ALL
  SELECT MyNumber + 1
  FROM xx
  WHERE xx.MyNumber <10
  )
SELECT * FROM xx
ORDER BY MyNumber;*/


SELECT table_name,string_agg(column_name) FROM Class_Test_Dataset.INFORMATION_SCHEMA.COLUMNS
GROUP BY table_name;
--WHERE table_name = 'outputevents';

SET @@dataset_id = 'Class_Test_Dataset';
--CREATE OR REPLACE PROCEDURE prep_data ()
--BEGIN


WITH demo as (SELECT subject_id,
  string_agg(DISTINCT insurance,'|') as insurance,
  string_agg(DISTINCT marital_status,'|') as marital_status,
  replace(replace(string_agg(DISTINCT ethnicity,'|'),'|UNKNOWN',''),'UNKNOWN|','') as ethnicity,
  max(deathtime) as deathtime,
  max(CASE
  WHEN deathtime is not NULL THEN 1
  ELSE 0
  END) as decease
  FROM admissions
  GROUP BY subject_id)
SELECT * FROM demo
LEFT JOIN patients on demo.subject_id=patients.subject_id;

/*
named_outputevents<-left_join(outputevents,d_items,by=c(itemid='itemid'))
named_labevents<-left_join(labevents,d_labitems)
named_chartevents<-left_join(chartevents,d_items)
named_diagnoses<-left_join(diagnoses_icd,d_icd_diagnoses)
*/

SET @@dataset_id = 'Class_Test_Dataset';

DROP TABLE IF EXISTS named_outputevents;
CREATE TABLE named_outputevents as
SELECT d_items.*,subject_id,hadm_id,stay_id,charttime,storetime,value,valueuom
FROM outputevents
LEFT JOIN d_items on outputevents.itemid = d_items.itemid;

DROP TABLE IF EXISTS named_labevents;
CREATE TABLE named_labevents as
SELECT d_labitems.*,labevent_id,subject_id,hadm_id,specimen_id,charttime,storetime,value,valuenum,valueuom,ref_range_lower,ref_range_upper,flag,priority,comments
FROM labevents
LEFT JOIN d_labitems on labevents.itemid = d_labitems.itemid;

DROP TABLE IF EXISTS named_chartevents;
CREATE TABLE named_chartevents as
SELECT d_items.*,subject_id,hadm_id,stay_id,charttime,storetime,value,valuenum,valueuom,warning
FROM chartevents
LEFT JOIN d_items on chartevents.itemid = d_items.itemid;

DROP TABLE IF EXISTS named_diagnoses;
CREATE TABLE named_diagnoses as
SELECT d_icd_diagnoses.*,subject_id,hadm_id
FROM diagnoses_icd
LEFT JOIN d_icd_diagnoses on diagnoses_icd.icd_code = d_icd_diagnoses.icd_code;


/*
  adm_scaffold = admissions %>% transmute( hadm_id = hadm_id, subject_id = subject_id,
                                      los = ceiling(as.numeric(dischtime - admittime) / 24),
                          date = purrr::map2(admittime,dischtime, function(xx,yy) seq(trunc(xx,units = 'days'),yy, by = 'day'))
                          ) %>% tidyr::unnest(date)

  */

  DROP TABLE IF EXISTS adm_scaffold;
  CREATE TABLE adm_scaffold as(
    WITH RECURSIVE q0 as(
      SELECT hadm_id,subject_id,Date(admittime) as hosp_date, Date(dischtime) as dischtime,
        date_diff(dischtime, admittime, day) as los
      FROM admissions
      UNION ALL
      SELECT hadm_id,subject_id,date_add(hosp_date, INTERVAL 1 day) as hosp_date, dischtime, los
      FROM q0
      WHERE hosp_date < dischtime
    )
    SELECT hadm_id,subject_id, hosp_date, los
    FROM q0
    ORDER BY hadm_id, hosp_date
  );
  SET @@dataset_id = 'Class_Test_Dataset';


/*
icu_Dates = icustays %>% transmute(hadm_id, subject_id, stay_id,
                                   ICUlos=los,
                                   ICUlos_revised = ceiling(as.numeric(outtime - intime) / 1440),
                                   ICU_date = purrr::map2(intime,outtime,
                       function(xx,yy) seq(trunc(xx,units = 'days'),yy, by = 'day'))) %>%
  tidyr::unnest(ICU_date) %>%
  group_by(hadm_id,subject_id,ICU_date) %>%
summarise(ICUlos = list(ICUlos),stay_id = list(stay_id))
*/

DROP TABLE IF EXISTS icu_scaffold;
CREATE TABLE icu_scaffold as (
 WITH RECURSIVE temp1 as (
 SELECT hadm_id, subject_id, stay_id, Date(intime)as ICUdate, ceiling (los) as ICUlos, ceiling(timestamp_diff(outtime, intime, minute)/1440) as ICUlosR, intime, Date(outtime) as outtime
 FROM icustays
 UNION ALL
 SELECT hadm_id,subject_id,stay_id,date_add(ICUdate, INTERVAL 1 day) as ICUdate, ICUlos, ICUlosR, intime, outtime
      FROM temp1
      WHERE ICUdate < outtime
 )
 SELECT *
 FROM temp1
 ORDER BY hadm_id, ICUdate
);

/*
# Combined admissions and ICU_Dates
MainData<-left_join(adm_Dates,icu_Dates, by=c("hadm_id"="hadm_id","subject_id"="subject_id","date"="ICU_date"))
*/

DROP TABLE IF EXISTS MainData;
CREATE TABLE MainData AS
  WITH temp2 AS (
    SELECT adm_scaffold.*,stay_id,ICUlos,
     ROW_NUMBER() OVER (PARTITION BY adm_scaffold.hadm_id, hosp_date ORDER BY intime) AS RN
    FROM adm_scaffold
    LEFT JOIN icu_scaffold on adm_scaffold.hadm_id=icu_scaffold.hadm_id AND adm_scaffold.hosp_date=icu_scaffold.ICUdate
    )
SELECT *
FROM temp2
WHERE RN=1
ORDER BY hadm_id,subject_id,hosp_date;

-- ROW_NUMBER start a window operation



--DYNAMIC SQL CODING
--Stored procedure
SET @@dataset_id = 'Class_Test_Dataset';
CREATE OR REPLACE PROCEDURE count_dup (
  tablenames STRING,columnnames STRING,selectname STRING, dataset STRING
)
BEGIN
 DECLARE mydataset STRING;
 DECLARE sqlquery STRING;
 SET mydataset = (SELECT COALESCE(dataset,'Class_Test_Dataset'));
 SET sqlquery = concat('SELECT ',columnnames,' FROM ',mydataset,'.',tablenames);
 SELECT sqlquery;
 EXECUTE IMMEDIATE sqlquery;


END;

CALL Class_Test_Dataset.count_dup('icu_scaffold','hadm_id, stay_id',null,null)



