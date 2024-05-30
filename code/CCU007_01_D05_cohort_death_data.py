# Databricks notebook source
# MAGIC %md
# MAGIC %md  
# MAGIC # CCU007_01_D05_cohort_death_data

# COMMAND ----------

# MAGIC %md
# MAGIC %md
# MAGIC **Description** Making a record of death among patients in the cohort
# MAGIC  
# MAGIC **Project(s)** CCU007_01 Descriptive paper
# MAGIC  
# MAGIC **Author(s)** Arun
# MAGIC  
# MAGIC
# MAGIC  
# MAGIC **Data input**  `dars_nic_391419_j3w9t_collab.deaths_dars_nic_391419_j3w9t_archive`; 
# MAGIC
# MAGIC **Data output** `dars_nic_391419_j3w9t_collab.ccu007_01_nicor_death_master_ak_221010`
# MAGIC
# MAGIC **Software and versions** Replace this with the software running the code in the notebook (and any version requirements), e.g. SQL, Python, R (>= 3.5.0)

# COMMAND ----------

# MAGIC %run Workspaces/dars_nic_391419_j3w9t_collab/CCU007_01/CCU007_01-functions/wrang000_functions

# COMMAND ----------

dbutils.widgets.removeAll();

# COMMAND ----------

database_name, collab_database_name, datawrang_database_name = database_widgets(database_name = 'dars_nic_391419_j3w9t', collab_database_name='dars_nic_391419_j3w9t_collab')

project_prefix = 'ccu007_01_'
skinny_table_name = 'nicor_death_master_'
initial =  'ak_221010'

# COMMAND ----------

# MAGIC %md
# MAGIC # Details of death from the NICOR dataset

# COMMAND ----------

spark.sql(f"""CREATE OR REPLACE GLOBAL TEMP VIEW ccu007_01_patient_skinny_single_patient_death AS

SELECT * 
FROM 
  (SELECT *, 
          to_date(REG_DATE_OF_DEATH, 'yyyyMMdd') as REG_DATE_OF_DEATH_FORMATTED, 
          to_date(REG_DATE, 'yyyyMMdd') as REG_DATE_FORMATTED, 
          row_number() OVER (PARTITION BY DEC_CONF_NHS_NUMBER_CLEAN_DEID 
                             ORDER BY REG_DATE desc, REG_DATE_OF_DEATH desc,
                             S_UNDERLYING_COD_ICD10 desc -- this is because you can have a situation where there are death records twice with the same dates, but only one has diagnoses... so we pick the one with the diagnoses
                             ) as death_rank,
                             LSOAR as LSOA
    FROM dars_nic_391419_j3w9t_collab.deaths_dars_nic_391419_j3w9t_archive
    
    ) cte
WHERE death_rank = 1
AND DEC_CONF_NHS_NUMBER_CLEAN_DEID IS NOT NULL
and REG_DATE_OF_DEATH_formatted > '1900-01-01'
AND REG_DATE_OF_DEATH_formatted <= current_date()
""")


# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW nicor_death AS
# MAGIC SELECT 
# MAGIC       *,      
# MAGIC      CASE WHEN DATE_OF_DEATH IS NULL OR TRIM(DATE_OF_DEATH) = "" OR DATE_OF_DEATH < '1900-01-01' OR DATE_OF_DEATH > current_date() OR DATE_OF_DEATH > RECORD_DATE THEN 1 ELSE 0 END as date_of_death_null,
# MAGIC       CASE WHEN dataset = 'death' THEN 1 ELSE 0 END as death_table
# MAGIC FROM (
# MAGIC SELECT *, DEC_CONF_NHS_NUMBER_CLEAN_DEID as NHS_NUMBER_DEID,
# MAGIC                 
# MAGIC                 REG_DATE_OF_DEATH_formatted as DATE_OF_DEATH,
# MAGIC                 REG_DATE_formatted as RECORD_DATE,
# MAGIC                
# MAGIC                 'death' as dataset,
# MAGIC                 0 as primary, 'death' as care_domain
# MAGIC               FROM global_temp.ccu007_01_patient_skinny_single_patient_death
# MAGIC )

# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW nicor_death_fields_ranked AS
# MAGIC SELECT * --NHS_NUMBER_DEID, DATE_OF_DEATH
# MAGIC FROM (
# MAGIC       SELECT *, row_number() OVER (PARTITION BY NHS_NUMBER_DEID 
# MAGIC                                     ORDER BY death_table desc,  RECORD_DATE DESC) as death_recency_rank
# MAGIC                
# MAGIC                               
# MAGIC       FROM global_temp.nicor_death
# MAGIC       WHERE dataset  <> "primary_SNOMED" -- <- this has the GDPPR SNOMED Ethnicity - we will just use the normal ones, which are in dataset = primary
# MAGIC       ) 

# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW nicor_death_id AS
# MAGIC SELECT DISTINCT 1_03_NHS_NUMBER_DEID 
# MAGIC FROM global_temp.nicor_child
# MAGIC
# MAGIC WHERE 4_09_ATTRIBUTION_OF_DEATH IS NOT NULL OR DATE_OF_DEATH IS NOT NULL OR 4_03_DISCHARGE_STATUS ="D. Died in hospital"
# MAGIC
# MAGIC ;

# COMMAND ----------

# MAGIC %sql
# MAGIC CREATE OR REPLACE GLOBAL TEMP VIEW ccu007_01_nicor_death_master AS
# MAGIC SELECT *
# MAGIC FROM  global_temp.nicor_death_id pat 
# MAGIC LEFT JOIN (SELECT  NHS_NUMBER_DEID, REG_DATE_OF_DEATH_FORMATTED, REG_DATE_FORMATTED,S_UNDERLYING_COD_ICD10, S_COD_CODE_1, S_COD_CODE_2, S_COD_CODE_3, S_COD_CODE_4, S_COD_CODE_5, S_COD_CODE_6, S_COD_CODE_7, S_COD_CODE_8, S_COD_CODE_9,
# MAGIC S_COD_CODE_10, S_COD_CODE_11, S_COD_CODE_12, S_COD_CODE_13, S_COD_CODE_14, S_COD_CODE_15, NEO_NATE_FLAG,S_UNDERLYING_COD_ICD9, ICD9_ORIG_MENTION_1, ICD9_ORIG_MENTION_2, ICD9_ORIG_MENTION_3, ICD9_ORIG_MENTION_4,
# MAGIC ICD9_ORIG_MENTION_5, ICD9_ORIG_MENTION_6, ICD9_ORIG_MENTION_7, ICD9_ORIG_MENTION_8, ICD9_ORIG_MENTION_9, ICD9_ORIG_MENTION_10, ICD9_ORIG_MENTION_11, ICD9_ORIG_MENTION_12, ICD9_ORIG_MENTION_13, ICD9_ORIG_MENTION_14, ICD9_ORIG_MENTION_15 FROM global_temp.nicor_death_fields_ranked WHERE death_recency_rank = 1 and death_table = 1) dod ON pat.1_03_NHS_NUMBER_DEID = dod.NHS_NUMBER_DEID;

# COMMAND ----------

def create_table(table_name:str, database_name:str='dars_nic_391419_j3w9t_collab', select_sql_script:str=None, if_not_exists=True) -> None:
  """Will save to table from a global_temp view of the same name as the supplied table name (if no SQL script is supplied)
  Otherwise, can supply a SQL script and this will be used to make the table with the specificed name, in the specifcied database."""
  
  spark.conf.set("spark.sql.legacy.allowCreatingManagedTableUsingNonemptyLocation","true")
  
  if select_sql_script is None:
    select_sql_script = f"SELECT * FROM global_temp.{table_name}"
  
  if if_not_exists is True:
    if_not_exists_script=' IF NOT EXISTS'
  else:
    if_not_exists_script=''
  
  spark.sql(f"""CREATE TABLE {if_not_exists_script} {database_name}.{table_name} AS
                {select_sql_script}
             """)
  spark.sql(f"ALTER TABLE {database_name}.{table_name} OWNER TO {database_name}")
  
def drop_table(table_name:str, database_name:str='dars_nic_391419_j3w9t_collab', if_exists=True):
  if if_exists:
    IF_EXISTS = 'IF EXISTS'
  else: 
    IF_EXISTS = ''
  spark.sql(f"DROP TABLE {IF_EXISTS} {database_name}.{table_name}")

# COMMAND ----------

spark.sql(f"""DROP TABLE IF EXISTS {collab_database_name}.ccu007_01_nicor_death_master_{initial}""")

# COMMAND ----------

create_table(project_prefix + skinny_table_name+initial, select_sql_script=f"SELECT * FROM global_temp.ccu007_01_nicor_death_master") 
