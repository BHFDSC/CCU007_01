# Databricks notebook source
# MAGIC %md # CCU007_01_patient_skinny_record: Make the Patient Skinny Record Table (all patient core facts in one place)

# COMMAND ----------

# MAGIC %md
# MAGIC **Description** Making a single record for each patient in primary and secondary care
# MAGIC  
# MAGIC **Project(s)** CCU007_01 Descriptive paper
# MAGIC  
# MAGIC **Author(s)** Arun
# MAGIC  
# MAGIC
# MAGIC  
# MAGIC **Data input** [HES, GDPPR, Deaths] `global_temp.ccu007_01_patient_skinny_unassembled`
# MAGIC
# MAGIC **Data output** `dars_nic_391419_j3w9t_collab.ccu007_01_nicor_patient_imd_ak_221010`
# MAGIC
# MAGIC **Software and versions** Replace this with the software running the code in the notebook (and any version requirements), e.g. SQL, Python, R (>= 3.5.0)
# MAGIC  
# MAGIC

# COMMAND ----------

# MAGIC %md # Making a single record for each patient in primary and secondary care

# COMMAND ----------

# MAGIC %md
# MAGIC This notebook will make a single record for each patient with the core facts about that patient, reconciled across the main datasets (primary and secondary care)
# MAGIC
# MAGIC |Column | Content|
# MAGIC |----------------|--------------------|
# MAGIC |NHS_NUMBER_DEID | Patient NHS Number |
# MAGIC |ETHNIC | Patient Ethnicity |
# MAGIC |SEX | Patient Sex |
# MAGIC |DATE_OF_BIRTH | Patient Date of Birth (month level) |
# MAGIC |DATE_OF_DEATH | Patient Date of Death (month level) |
# MAGIC |record_id | The id of the record from which the data was drawn |
# MAGIC |dataset | The dataset from which the record comes from |
# MAGIC |primary | Whether the record refers to primary of secondary care |

# COMMAND ----------

# MAGIC %run Workspaces/dars_nic_391419_j3w9t_collab/CCU007_01/CCU007_01-functions/wrang000_functions

# COMMAND ----------

dbutils.widgets.removeAll()

# COMMAND ----------

database_name, collab_database_name, datawrang_database_name = database_widgets(database_name = 'dars_nic_391419_j3w9t', collab_database_name='dars_nic_391419_j3w9t_collab')

project_prefix = 'ccu007_01_'
skinny_table_name = 'nicor_patient_imd' 
initial ='_ak_221010'



# COMMAND ----------

# MAGIC %md ## Handle the multiple versions of the truth

# COMMAND ----------

# MAGIC %md Choose the appropriate values so that we have one version of the truth for each Patient.
# MAGIC
# MAGIC Currently, that simply involves picking the **most recent record**:
# MAGIC - choosing a populated field first
# MAGIC - choosing from primary care first if possible 
# MAGIC -  and then only choosing from secondary or null values if no other available).

# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW ccu007_01_patient_fields_ranked AS
# MAGIC SELECT * --NHS_NUMBER_DEID, DATE_OF_DEATH
# MAGIC FROM (
# MAGIC       SELECT *, row_number() OVER (PARTITION BY NHS_NUMBER_DEID 
# MAGIC                                     ORDER BY death_table desc,  RECORD_DATE DESC) as death_recency_rank,
# MAGIC                 row_number() OVER (PARTITION BY NHS_NUMBER_DEID 
# MAGIC                                     ORDER BY date_of_birth_null asc, primary desc, RECORD_DATE DESC) as birth_recency_rank,
# MAGIC                 row_number() OVER (PARTITION BY NHS_NUMBER_DEID 
# MAGIC                                     ORDER BY sex_null asc, primary desc, RECORD_DATE DESC) as sex_recency_rank,
# MAGIC                 row_number() OVER (PARTITION BY NHS_NUMBER_DEID 
# MAGIC                                     ORDER BY ethnic_null asc, primary desc, RECORD_DATE DESC) as ethnic_recency_rank,
# MAGIC                 row_number() OVER (PARTITION BY NHS_NUMBER_DEID 
# MAGIC                                     ORDER BY lsoa_null asc, primary desc, RECORD_DATE DESC) as lsoa_recency_rank
# MAGIC                               
# MAGIC       FROM global_temp.ccu007_01_patient_skinny_unassembled
# MAGIC       WHERE dataset  <> "primary_SNOMED" -- <- this has the GDPPR SNOMED Ethnicity - we will just use the normal ones, which are in dataset = primary
# MAGIC       ) 

# COMMAND ----------

# MAGIC %md ### Assemble the columns together to make skinny record

# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW ccu007_01_patient_skinny_record AS
# MAGIC SELECT 
# MAGIC        pat.NHS_NUMBER_DEID,
# MAGIC       eth.ETHNIC,
# MAGIC       sex.SEX,
# MAGIC       lso.LSOA,
# MAGIC       dob.DATE_OF_BIRTH,
# MAGIC       dod.DATE_OF_DEATH
# MAGIC FROM (SELECT DISTINCT NHS_NUMBER_DEID FROM global_temp.ccu007_01_patient_skinny_unassembled) pat 
# MAGIC         INNER JOIN (SELECT NHS_NUMBER_DEID, ETHNIC FROM global_temp.ccu007_01_patient_fields_ranked WHERE ethnic_recency_rank = 1) eth ON pat.NHS_NUMBER_DEID = eth.NHS_NUMBER_DEID
# MAGIC         INNER JOIN (SELECT NHS_NUMBER_DEID, SEX FROM global_temp.ccu007_01_patient_fields_ranked WHERE sex_recency_rank = 1) sex ON pat.NHS_NUMBER_DEID = sex.NHS_NUMBER_DEID
# MAGIC         INNER JOIN (SELECT NHS_NUMBER_DEID, DATE_OF_BIRTH FROM global_temp.ccu007_01_patient_fields_ranked WHERE birth_recency_rank = 1) dob ON pat.NHS_NUMBER_DEID = dob.NHS_NUMBER_DEID
# MAGIC         LEFT JOIN (SELECT NHS_NUMBER_DEID, LSOA FROM global_temp.ccu007_01_patient_fields_ranked WHERE lsoa_recency_rank = 1) lso ON pat.NHS_NUMBER_DEID = lso.NHS_NUMBER_DEID
# MAGIC         LEFT JOIN (SELECT NHS_NUMBER_DEID, DATE_OF_DEATH FROM global_temp.ccu007_01_patient_fields_ranked WHERE death_recency_rank = 1 and death_table = 1) dod ON pat.NHS_NUMBER_DEID = dod.NHS_NUMBER_DEID

# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW skinny1 AS
# MAGIC SELECT NHS_NUMBER_DEID as PERSON_ID, ETHNIC, SEX, LSOA, DATE_OF_BIRTH, DATE_OF_DEATH
# MAGIC FROM global_temp.ccu007_01_patient_skinny_record;

# COMMAND ----------

# MAGIC %sql
# MAGIC
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW ccu007_01_nicor_patient_record AS
# MAGIC SELECT *
# MAGIC FROM dars_nic_391419_j3w9t.nicor_congenital_dars_nic_391419_j3w9t as nicor
# MAGIC LEFT JOIN global_temp.skinny1
# MAGIC ON nicor.1_03_NHS_NUMBER_DEID = skinny1.PERSON_ID; 

# COMMAND ----------

# MAGIC %sql
# MAGIC
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW ccu007_01_nicor_patient_imd AS
# MAGIC SELECT pat.*, lsoa_ind.LSOA_CODE_2011, lsoa_ind.DECI_IMD, lsoa_reg.lsoa_code, lsoa_reg.region_name
# MAGIC
# MAGIC FROM global_temp.ccu007_01_nicor_patient_record as pat
# MAGIC LEFT JOIN (SELECT DISTINCT LSOA_CODE_2011, DECI_IMD, row_number() OVER (PARTITION BY LSOA_CODE_2011 ORDER BY IMD_YEAR desc) AS max_date FROM dss_corporate.english_indices_of_dep_v02) as lsoa_ind ON lsoa_ind.LSOA_CODE_2011=pat.LSOA AND max_date=1
# MAGIC LEFT JOIN (SELECT DISTINCT lsoa_code, region_name FROM dars_nic_391419_j3w9t_collab.curr901a_lsoa_region_lookup) as lsoa_reg ON lsoa_reg.lsoa_code=pat.LSOA;

# COMMAND ----------

spark.sql(f"""DROP TABLE IF EXISTS {collab_database_name}.{project_prefix}{skinny_table_name}{initial}""")

# COMMAND ----------

create_table(project_prefix + skinny_table_name+initial, select_sql_script=f"SELECT * FROM global_temp.{project_prefix}{skinny_table_name}") 
