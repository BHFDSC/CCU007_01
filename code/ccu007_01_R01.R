#=======================================================
#CCU007_01 : Description of the NICOR dataset
# Date : 01 Dec 2021
  
  

# loading the dataset from databricks -------------------------------------



library(DBI)
con <- dbConnect(odbc::odbc(), "Databricks", timeout = 60, PWD = rstudioapi::askForPassword("Please enter your Databricks personal access token"))




#  dataset ---------------------------------------

data <- dbGetQuery(con,'SELECT * FROM dars_nic_391419_j3w9t_collab.ccu007_01_nicor_patient_imd_ak_221010')

death_nicor <- dbGetQuery(con,'SELECT * FROM dars_nic_391419_j3w9t_collab.ccu007_01_nicor_death_master_ak_221010')


setwd("/mnt/efs/arun.k.suseeladevi/CCU007_01")




setwd("~/CCU007_01/data")


write.csv(data, "ccu007_01_data_extract.csv" ) 

write.csv(death_nicor, "ccu007_01_deathdata_extract.csv" ) 



