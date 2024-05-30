# Databricks notebook source
# MAGIC %md  # CCU007_01-D00-project_table_freeze
# MAGIC
# MAGIC **Description** This notebook extracts the data from specified time point (batchId) and then applies a specified common cutoff date (i.e. any records beyond this time are dropped).
# MAGIC
# MAGIC **Author(s)** Updated by Arun from work by Rochelle Knight for CCU002_01 from notebook by Sam Hollings and Jenny Cooper 
# MAGIC
# MAGIC **Project(s)** CCU007_01
# MAGIC
# MAGIC **Reviewer(s)** 
# MAGIC
# MAGIC **Date last updated** 2021-06-25
# MAGIC
# MAGIC **Date last reviewed**
# MAGIC
# MAGIC **Data last run** 2021-06-25
# MAGIC
# MAGIC **Data input** 
# MAGIC `dars_nic_391419_j3w9t.gdppr_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC
# MAGIC
# MAGIC `dars_nic_391419_j3w9t.deaths_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.hes_ae_all_years`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.hes_op_all_years`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.hes_apc_all_years`
# MAGIC
# MAGIC
# MAGIC
# MAGIC **Data output** 
# MAGIC
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_deaths_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_gdppr_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_hes_ae_all_years`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_hes_apc_all_years`
# MAGIC
# MAGIC
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_hes_op_all_years`
# MAGIC
# MAGIC
# MAGIC **Software and versions** SQL, Python
# MAGIC
# MAGIC **Packages and versions** Not applicable

# COMMAND ----------

# MAGIC %run "/Workspaces/dars_nic_391419_j3w9t_collab/CCU007/CCU007_01-functions/wrang000_functions"

# COMMAND ----------

# MAGIC %run "/Workspaces/dars_nic_391419_j3w9t_collab/CCU007/CCU007_01-D01-master_notebook_parameters"

# COMMAND ----------

# Parameters

project_prefix = 'ccu007_01_'
collab_database_name = 'dars_nic_391419_j3w9t_collab'

# COMMAND ----------

import datetime 
import pandas as pd

batch_id = None
copy_date = datetime.datetime.now()


# COMMAND ----------

df_tables_list = spark.table(f'{collab_database_name}.wrang005_asset_inventory').toPandas().sort_values(['core_asset','tableName'],ascending=[False,True])

# COMMAND ----------

display(df_tables_list)

# COMMAND ----------

df_freeze_table_list = pd.DataFrame([
             {'tableName':'gdppr_dars_nic_391419_j3w9t','extra_columns':'','date_cutoff_col':'DATE', 'ignore_cutoff': True,'batch_id': None},
             {'tableName':'deaths_dars_nic_391419_j3w9t','extra_columns':", to_date(REG_DATE_OF_DEATH, 'yyyyMMdd') as REG_DATE_OF_DEATH_FORMATTED, to_date(REG_DATE, 'yyyyMMdd') as REG_DATE_FORMATTED",'date_cutoff_col':"REG_DATE_OF_DEATH_FORMATTED", 'ignore_cutoff': True,'batch_id': None},
             {'tableName':'hes_apc_all_years','extra_columns':'','date_cutoff_col':'ADMIDATE', 'ignore_cutoff': True,'batch_id': None},
             {'tableName': 'hes_op_all_years','extra_columns':'','date_cutoff_col':'APPTDATE', 'ignore_cutoff': True,'batch_id': None},
             {'tableName': 'hes_ae_all_years','extra_columns':'','date_cutoff_col':'ARRIVALDATE', 'ignore_cutoff': True,'batch_id': None}
])

# insert the above batch ID if not specified in the df_freeze_Table_list
if batch_id is not None:
  df_freeze_table_list = df_freeze_table_list.fillna(value={'batch_id':batch_id})

# COMMAND ----------

pd.DataFrame(df_freeze_table_list)

# COMMAND ----------

# MAGIC %md get the max batch Id for each table which doesn't already have a batchId specified:

# COMMAND ----------

get_max_batch_id = lambda x: spark.table(x['archive_path']).select('ProductionDate','BatchId').distinct().orderBy('ProductionDate', ascending=False).toPandas().loc[0,'BatchId']

df_tables = (df_tables_list.merge(pd.DataFrame(df_freeze_table_list), left_on='tableName', right_on='tableName', how='inner'))
null_batch_id_index = df_tables['batch_id'].isna()
df_tables.loc[null_batch_id_index,'batch_id'] = df_tables.loc[null_batch_id_index].apply(get_max_batch_id, axis=1)

df_tables

# COMMAND ----------

# MAGIC %md **make the frozen tables** : go through the table of tables taking the records from the archive for specified batch, and putting them in a new table called after the old table with the specified prefix.

# COMMAND ----------

import pandas as pd
from pyspark.sql.functions import to_date, lit

error_list = []

for idx, row in df_tables.iterrows():
  try:
    table_name = row.tableName 
    cutoff_col = row.date_cutoff_col
    extra_columns_sql = row.extra_columns
    batch_id = row['batch_id']
    #print(batch_id)
    
    print('---- ', table_name)
    sdf_table = spark.sql(f"""SELECT '{copy_date}' as ProjectCopyDate,  
                            * {extra_columns_sql} FROM {collab_database_name}.{table_name}_archive""")
    
    if row['ignore_cutoff'] is False:
      sdf_table_cutoff = sdf_table.filter(f"""{cutoff_col} <= '{cutoff}'
                                        AND BatchId = '{batch_id}'""") 
    elif row['ignore_cutoff'] is True:
      sdf_table_cutoff = sdf_table.filter(f"""BatchId = '{batch_id}'""") 
    else:
        raise ValueError(f'table: {table_name},  ignore_cutoff  needs either a True or False value')

    
    sdf_table_cutoff.createOrReplaceGlobalTempView(f"{project_prefix}{table_name}")
    print(f'    ----> Made: global_temp.{project_prefix}{table_name}')
    source_table = f"global_temp.{project_prefix}{table_name}"
    current_table = f"{collab_database_name}.{project_prefix}{table_name}"
    destination_table = f"{collab_database_name}.{project_prefix}{table_name}"

    spark.sql(f"DROP TABLE IF EXISTS {current_table}")

    spark.sql(f"""CREATE TABLE IF NOT EXISTS {destination_table} AS 
                  SELECT * FROM {source_table} WHERE FALSE""")

    spark.sql(f"""ALTER TABLE {destination_table} OWNER TO {collab_database_name}""")

    spark.sql(f"""
              TRUNCATE TABLE {destination_table}
              """)

    spark.sql(f"""
             INSERT INTO {destination_table}
              SELECT * FROM {source_table}
              """)

    print(f'    ----> Made: {destination_table}')
    
  except Exception as error:
    print(table, ": ", error)
    error_list.append(table)
    print()

print(error_list)

# D02

# Databricks notebook source
# MAGIC %md  # CCU007_01-D00-project_table_freeze
# MAGIC
# MAGIC **Description** This notebook extracts the data from specified time point (batchId) and then applies a specified common cutoff date (i.e. any records beyond this time are dropped).
# MAGIC
# MAGIC **Author(s)** Updated by Arun from work by Rochelle Knight for CCU002_01 from notebook by Sam Hollings and Jenny Cooper 
# MAGIC
# MAGIC **Project(s)** CCU007_01
# MAGIC
# MAGIC **Reviewer(s)** 
# MAGIC
# MAGIC **Date last updated** 2021-06-25
# MAGIC
# MAGIC **Date last reviewed**
# MAGIC
# MAGIC **Data last run** 2021-06-25
# MAGIC
# MAGIC **Data input** 
# MAGIC `dars_nic_391419_j3w9t.gdppr_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC
# MAGIC
# MAGIC `dars_nic_391419_j3w9t.deaths_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.hes_ae_all_years`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.hes_op_all_years`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.hes_apc_all_years`
# MAGIC
# MAGIC
# MAGIC
# MAGIC **Data output** 
# MAGIC
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_deaths_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_gdppr_dars_nic_391419_j3w9t`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_hes_ae_all_years`
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_hes_apc_all_years`
# MAGIC
# MAGIC
# MAGIC
# MAGIC `dars_nic_391419_j3w9t_collab.ccu007_01_hes_op_all_years`
# MAGIC
# MAGIC
# MAGIC **Software and versions** SQL, Python
# MAGIC
# MAGIC **Packages and versions** Not applicable

# COMMAND ----------

# MAGIC %run "/Workspaces/dars_nic_391419_j3w9t_collab/CCU007/CCU007_01-functions/wrang000_functions"

# COMMAND ----------

# MAGIC %run "/Workspaces/dars_nic_391419_j3w9t_collab/CCU007/CCU007_01-D01-master_notebook_parameters"

# COMMAND ----------

# Parameters

project_prefix = 'ccu007_01_'
collab_database_name = 'dars_nic_391419_j3w9t_collab'

# COMMAND ----------

import datetime 
import pandas as pd

batch_id = None
copy_date = datetime.datetime.now()


# COMMAND ----------

df_tables_list = spark.table(f'{collab_database_name}.wrang005_asset_inventory').toPandas().sort_values(['core_asset','tableName'],ascending=[False,True])

# COMMAND ----------

display(df_tables_list)

# COMMAND ----------

df_freeze_table_list = pd.DataFrame([
             {'tableName':'gdppr_dars_nic_391419_j3w9t','extra_columns':'','date_cutoff_col':'DATE', 'ignore_cutoff': True,'batch_id': None},
             {'tableName':'deaths_dars_nic_391419_j3w9t','extra_columns':", to_date(REG_DATE_OF_DEATH, 'yyyyMMdd') as REG_DATE_OF_DEATH_FORMATTED, to_date(REG_DATE, 'yyyyMMdd') as REG_DATE_FORMATTED",'date_cutoff_col':"REG_DATE_OF_DEATH_FORMATTED", 'ignore_cutoff': True,'batch_id': None},
             {'tableName':'hes_apc_all_years','extra_columns':'','date_cutoff_col':'ADMIDATE', 'ignore_cutoff': True,'batch_id': None},
             {'tableName': 'hes_op_all_years','extra_columns':'','date_cutoff_col':'APPTDATE', 'ignore_cutoff': True,'batch_id': None},
             {'tableName': 'hes_ae_all_years','extra_columns':'','date_cutoff_col':'ARRIVALDATE', 'ignore_cutoff': True,'batch_id': None}
])

# insert the above batch ID if not specified in the df_freeze_Table_list
if batch_id is not None:
  df_freeze_table_list = df_freeze_table_list.fillna(value={'batch_id':batch_id})

# COMMAND ----------

pd.DataFrame(df_freeze_table_list)

# COMMAND ----------

# MAGIC %md get the max batch Id for each table which doesn't already have a batchId specified:

# COMMAND ----------

get_max_batch_id = lambda x: spark.table(x['archive_path']).select('ProductionDate','BatchId').distinct().orderBy('ProductionDate', ascending=False).toPandas().loc[0,'BatchId']

df_tables = (df_tables_list.merge(pd.DataFrame(df_freeze_table_list), left_on='tableName', right_on='tableName', how='inner'))
null_batch_id_index = df_tables['batch_id'].isna()
df_tables.loc[null_batch_id_index,'batch_id'] = df_tables.loc[null_batch_id_index].apply(get_max_batch_id, axis=1)

df_tables

# COMMAND ----------

# MAGIC %md **make the frozen tables** : go through the table of tables taking the records from the archive for specified batch, and putting them in a new table called after the old table with the specified prefix.

# COMMAND ----------

import pandas as pd
from pyspark.sql.functions import to_date, lit

error_list = []

for idx, row in df_tables.iterrows():
  try:
    table_name = row.tableName 
    cutoff_col = row.date_cutoff_col
    extra_columns_sql = row.extra_columns
    batch_id = row['batch_id']
    #print(batch_id)
    
    print('---- ', table_name)
    sdf_table = spark.sql(f"""SELECT '{copy_date}' as ProjectCopyDate,  
                            * {extra_columns_sql} FROM {collab_database_name}.{table_name}_archive""")
    
    if row['ignore_cutoff'] is False:
      sdf_table_cutoff = sdf_table.filter(f"""{cutoff_col} <= '{cutoff}'
                                        AND BatchId = '{batch_id}'""") 
    elif row['ignore_cutoff'] is True:
      sdf_table_cutoff = sdf_table.filter(f"""BatchId = '{batch_id}'""") 
    else:
        raise ValueError(f'table: {table_name},  ignore_cutoff  needs either a True or False value')

    
    sdf_table_cutoff.createOrReplaceGlobalTempView(f"{project_prefix}{table_name}")
    print(f'    ----> Made: global_temp.{project_prefix}{table_name}')
    source_table = f"global_temp.{project_prefix}{table_name}"
    current_table = f"{collab_database_name}.{project_prefix}{table_name}"
    destination_table = f"{collab_database_name}.{project_prefix}{table_name}"

    spark.sql(f"DROP TABLE IF EXISTS {current_table}")

    spark.sql(f"""CREATE TABLE IF NOT EXISTS {destination_table} AS 
                  SELECT * FROM {source_table} WHERE FALSE""")

    spark.sql(f"""ALTER TABLE {destination_table} OWNER TO {collab_database_name}""")

    spark.sql(f"""
              TRUNCATE TABLE {destination_table}
              """)

    spark.sql(f"""
             INSERT INTO {destination_table}
              SELECT * FROM {source_table}
              """)

    print(f'    ----> Made: {destination_table}')
    
  except Exception as error:
    print(table, ": ", error)
    error_list.append(table)
    print()

print(error_list)

