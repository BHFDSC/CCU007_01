###################################################################################################
## NCHDA activity analysis algorithm s
###################################################################################################


library(zoo)
library(readr)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)


algorithm_version <- '6.14'


setwd("~/CCU007_01/scripts")

###################################################################################################

source('02.nchda_aa_sp_shared_codes.R')


#############################################################################################
#Data preparation



data <- read_csv("~/CCU007_01/data/ccu007_01_data_extract.csv")

data2 <-  data %>% 
  mutate(visit_dt=as.Date(`3_01_DATE_OF_VISIT`),
         dob=as.Date(DATE_OF_BIRTH),
         age=(visit_dt-dob)/365.25,
         child=ifelse(age<16,1,0)) %>% 
  dplyr::filter(child==1)
data2 <- data2 %>% 
  dplyr::mutate(proc_hr=hour(`3_01_DATE_OF_VISIT`),
                proc_min=minute(`3_01_DATE_OF_VISIT`),
                proc_time=paste(proc_hr, proc_min, sep=":")) %>% 
  dplyr::mutate(uniq1=paste(`1_03_NHS_NUMBER_DEID`, visit_dt),
                uniq2=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time),
                uniq3=paste(`1_03_NHS_NUMBER_DEID`,visit_dt, proc_time),
                uniq4=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`),
                uniq5=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`, `4_01_DATE_OF_DISCHARGE` ))

x3 <-  data2 %>% 
  dplyr::mutate(procd=`3_09_OPERATION_PERFORMED`,
                type_procedure_string=`3_07_TYPE_OF_PROCEDURE`) %>% 
  dplyr::select("1_03_NHS_NUMBER_DEID", visit_dt, procd,"3_09_OPERATION_PERFORMED","8_01_PROCEDURE_UNIQUE_ID","4_01_DATE_OF_DISCHARGE","proc_time",uniq4,age,
                type_procedure_string)
x5 <- x3 %>% 
  tidyr::separate(procd, c("proc_1",  "proc_2", "proc_3", "proc_4", "proc_5", "proc_6", "proc_7", "proc_8"), ";")

x6 <-  x5 %>% 
  separate(proc_1, c("procedure_1", "proc1")) %>% 
  separate(proc_2, c("procedure_2", "proc2")) %>% 
  separate(proc_3, c("procedure_3", "proc3")) %>% 
  separate(proc_4, c("procedure_4", "proc4")) %>% 
  separate(proc_5, c("procedure_5", "proc5")) %>% 
  separate(proc_6, c("procedure_6", "proc6")) %>% 
  separate(proc_7, c("procedure_7", "proc7")) %>% 
  separate(proc_8, c("procedure_8", "proc8")) %>% 
  separate(type_procedure_string, c("type_procedure","type_str"))
nchda <- x6
nchda_analysis <- nchda

nchda_analysis <- plyr:::rename(nchda_analysis, c('age'='age_years', 'visit_dt' = 'procedure_date',
                                                  '1_03_NHS_NUMBER_DEID' = 'patient_identifier'))

## add fields for summary/processing
nchda_analysis[,'age_group'] <- ifelse((nchda_analysis[,'age_years'] < 16.00), 'paediatric', 'adult') 



nchda_analysis$type_procedure <- as.numeric(nchda_analysis$type_procedure)

nchda_analysis[,'type_procedure'][is.na(nchda_analysis[,'type_procedure'])] <- 0

nchda_analysis[,'report_group'] <- '' 


nchda_analysis[,'algorithm_version'] <- algorithm_version 

nchda_analysis <- nchda_analysis %>% 
  dplyr::arrange(patient_identifier, procedure_date,type_procedure )


nchda_analysis <- nchda_analysis %>%
  dplyr::mutate(c1= procedure_1,c2= procedure_2,
                c3= procedure_3,c4= procedure_4,
                c5= procedure_5,c6= procedure_6,
                c7= procedure_7)


## convert any empty strings '' to NA

nchda_analysis[, c('c1','c2','c3','c4','c5','c6','c7')][nchda_analysis[, c('c1','c2','c3','c4','c5','c6','c7')] == ''] <- NA      



###################################################################################################
## declare the inclusion/exclusion lists

## surgical
vad <- c('123704','128721','128722','128723','128724','128741')


## surgical
primary_ecmo_include <- c('128725','128726','128727')


## catheter
icd <- c('124231','124233','124234','124235','124239','124261','124264','124265','124279')


## catheter
pacemaker <- c('123450','123451','123452','123460','123463','123464','123467','123468','123470',
               '123473','123484','123485','123513','123514','124370','124475')


## catheter
ep <- c('123546','123548','123557','123582','123583','123584','123840','123869','130512','130517')


## catheter
diagnostic <- c('120625','122341','124501','124507','130024','130501','130505',
                '130506','130507','130508','130513','130124','130127')


###################################################################################################
## process data > fall through is classified as unallocated
## ecmo is then reprocessed to ascertain primary e    ###############################################################################################
## allocate code list and exclude any invalid procedure codes (interseccmo
main_allocation <- function(nchda_main) {
  for(i in 1:nrow(nchda_main)) {
    ###############################################################################################
    ## allocate code list and exclude any invalid procedure codes (intersect - procedures_valid) 
    ## remove any valid codes that are ignored (setdiff - procedures_exclude) or minor_excluded
    ## use default dplyr version of setdiff > loaded last
    #code_list <- (nchda_analysis[i,c('c1','c2','c3','c4','c5','c6','c7','c8')])
    code_list <- (nchda_analysis[i, c('c1','c2','c3','c4','c5','c6','c7')])
    
    ## drop any invalid procedures
    code_list <- base::intersect(code_list, procedures_valid_nchda)
    
    ## remove any procedure codes excluded from the NCHDA list
    code_list <- setdiff(code_list, procedures_exclude_non_nchda)
    
    ## remove any procedure codes excluded from the NCHDA listed in minor_and_excluded list
    code_list <- setdiff(code_list, minor_and_excluded_algorithm) 
    
    ## remove any NA codes from list
    code_list <- code_list[!is.na(code_list)]
    
    ## there should only be valid procedure codes having removed invalid/excluded and minor_and_excluded codes
    
    
    ###############################################################################################
    # Step 0: excluded (no valid codes present)
    if(length(code_list) == 0)
    {
      nchda_main[i,'report_group'] <- 'no_qualifying_codes'
      ## print(paste('record ',i,'- Step 0:no_qualifying_codes'))
    }
    
    
    ###############################################################################################    
    ## Step 1: bypass
    ## ensure no ecmo/vad included - so remove from code_list and then check for being empty (only ecmo/vad left)
    ## required because no longer using minor_and_excluded in SP
    code_list_remaining <- setdiff(code_list, primary_ecmo_include)
    code_list_remaining <- setdiff(code_list_remaining, vad)
    
    if((nchda_main[i,'type_procedure'] == 1) 
       & (nchda_main[i,'report_group'] == '')
       & (length(code_list_remaining) > 0))
    {
      nchda_main[i,'report_group'] <- 'bypass'
      ## print(paste('record ',i,'- Step 1:bypass'))
    }
    
    
    ###############################################################################################    
    ## Step 2: non-bypass
    ## ensure no ecmo/vad included - so remove from code_list and then check for being empty (only ecmo/vad left)
    ## required because no longer using minor_and_excluded in SP
    code_list_remaining <- setdiff(code_list, primary_ecmo_include)
    code_list_remaining <- setdiff(code_list_remaining, vad)
    
    if((nchda_main[i,'type_procedure'] %in% c(2,4,6,11) == TRUE)
       & (nchda_main[i,'report_group'] == '') 
       & (length(code_list_remaining) > 0))
    {
      nchda_main[i,'report_group'] <- 'non-bypass'
      ## print(paste('record ',i,'- Step 2:non-bypass'))
    }
    
    
    ###############################################################################################    
    ## Step 3: hybrid
    code_list_remaining <- setdiff(code_list, primary_ecmo_include)
    code_list_remaining <- setdiff(code_list_remaining, vad)
    
    if((nchda_main[i,'type_procedure'] == 7) 
       & (nchda_main[i,'report_group'] == '')
       & (length(code_list_remaining) > 0)) 
    {
      nchda_main[i,'report_group'] <- 'hybrid'
      ## print(paste('record ',i,'- Step 3:hybrid')) 
    }
    
    
    ###############################################################################################    
    ## Step 4: vad
    if((nchda_main[i,'type_procedure'] %in% c(1,2,4,6) == TRUE)
       & (nchda_main[i,'report_group'] == '')
       & (any(vad %in% code_list)))
    {
      nchda_main[i,'report_group'] <- 'vad'
      ## print(paste('record ',i,'- Step 4:vad')) 
    }
    
    
    ###############################################################################################    
    ## Step 5: set all possible ecmo > ecmo & then update values by iteration
    if((nchda_main[i,'type_procedure'] %in% c(1,2,4,6,7) == TRUE) 
       & (nchda_main[i,'report_group'] == '')
       & (any(primary_ecmo_include %in% code_list) == TRUE))      
    {
      nchda_main[i,'report_group'] <- 'ecmo' 
      ## print(paste('record ',i,'- Step 5:all ecmo'))
    }
    
    
    ###############################################################################################    
    ## Step 6: icd (non-surgical)
    code_list_remaining <- setdiff(code_list, icd)
    code_list_remaining <- setdiff(code_list_remaining, pacemaker)
    code_list_remaining <- setdiff(code_list_remaining, ep)
    code_list_remaining <- setdiff(code_list_remaining, diagnostic)
    
    if((nchda_main[i,'type_procedure']  %in% c(3,5,10) == TRUE) 
       & (nchda_main[i,'report_group'] == '')
       & (any(icd %in% code_list))
       & (length(code_list_remaining) == 0))
    {
      nchda_main[i,'report_group'] <- 'icd:non-surgical'
      ## print(paste('record ',i,'- Step 6:icd'))
    } 
    
    
    ###############################################################################################    
    ## Step 7: pacemaker (non-surgical) 
    code_list_remaining <- setdiff(code_list, pacemaker)
    code_list_remaining <- setdiff(code_list_remaining, ep)    
    code_list_remaining <- setdiff(code_list_remaining, diagnostic)
    
    if((nchda_main[i,'type_procedure']  %in% c(3,5,10) == TRUE) 
       & (nchda_main[i,'report_group'] == '')
       & (any(pacemaker %in% code_list))
       & (length(code_list_remaining) == 0))
    {
      nchda_main[i,'report_group'] <- 'pacemaker:non-surgical'
      ## print(paste('record ',i,'- Step 7:pacing'))
    }  
    
    
    ###############################################################################################    
    ## Step 8: ep (non-surgical)
    code_list_remaining <- setdiff(code_list, ep)    
    code_list_remaining <- setdiff(code_list_remaining, diagnostic) 
    
    if((nchda_main[i,'type_procedure']  %in% c(3,5,10) == TRUE) 
       & (nchda_main[i,'report_group'] == '')
       & (any(ep %in% code_list))
       & (length(code_list_remaining) == 0))
    {
      nchda_main[i,'report_group'] <- 'ep:non-surgical'
      ## print(paste('record ',i,'- Step 8:ep'))
    }     
    
    
    ###############################################################################################    
    ## Step 9: intervention (non-surgical)
    code_list_remaining <- setdiff(code_list, diagnostic)
    
    if((nchda_main[i,'type_procedure']  %in% c(3,5) == TRUE) 
       & (nchda_main[i,'report_group'] == '')
       & (length(code_list_remaining) > 0))
    {
      nchda_main[i,'report_group'] <- 'intervention:non-surgical'
      ## print(paste('record ',i,'- Step 9:intervention'))
    }     
    
    
    ###############################################################################################    
    ## Step 10: diagnostic (non-surgical)
    code_list_remaining <- setdiff(code_list, diagnostic)
    
    if((nchda_main[i,'type_procedure'] %in% c(3,5) == TRUE) 
       & (nchda_main[i,'report_group'] == '')
       & (any(diagnostic %in% code_list) == TRUE)
       & (length(code_list_remaining) == 0))
    {
      nchda_main[i,'report_group'] <- 'diagnostic:non-surgical'
      ## print(paste('record ',i,'- Step 10:diagnostic'))
    }
    
    
    ###############################################################################################    
    ## Step 11: set remaining to unallocated
    if(nchda_main[i,'report_group'] == '')
    {
      nchda_main[i,'report_group'] <- 'unallocated'
      ## print(paste('record ',i,'- Step 11:unallocated'))
    }
    ###############################################################################################
  }
  return(nchda_main)
}



###################################################################################################
## primary ecmo
ecmo_allocation <- function(nchda_ecmo) {
  for(i in 1:nrow(nchda_ecmo)) {
    ## first patient > cannot check preceding patient
    if (i == 1)
    {
      if (nchda_ecmo[i,'report_group'] == 'ecmo')
      {
        nchda_ecmo[i,'report_group'] <- 'primary_ecmo'
        ## print(paste('record ',i,'- Step 12:primary_ecmo'))
      }
    }  
    else
    {
      ## all subesequent records
      ## if same patient
      if ((nchda_ecmo[i,'patient_identifier']) == (nchda_ecmo[(i - 1),'patient_identifier']))
      {
        ## check that record is ecmo 
        ## & previous record is not ecmo
        ## & procedures are > 30 days apart
        if((nchda_ecmo[i,'report_group'] == 'ecmo')
           & (nchda_ecmo[(i - 1),'report_group'] != 'ecmo')
           & ((nchda_ecmo[i,'procedure_date']) - (nchda_ecmo[(i - 1) ,'procedure_date']) > 30)
        )
        {
          nchda_ecmo[i,'report_group'] <- 'primary_ecmo' 
          ## print(paste('record ',i,'- Step 12:primary_ecmo'))
        }
      }
      else
      {
        ## different patient & first record for that patient - if ecmo then must be primary_ecmo
        if (nchda_ecmo[i,'report_group'] == 'ecmo')
        {
          nchda_ecmo[i,'report_group'] <- 'primary_ecmo' 
          ## print(paste('record ',i,'- Step 12:primary_ecmo'))
        }        
      }
    }
  }
  ###############################################################################################          
  return(nchda_ecmo)
}


###################################################################################################
## run both process & then update values
nchda_analysis <- main_allocation(nchda_analysis)
# setwd("/mnt/efs/arun.k.suseeladevi/CCU007_01/data")
# write.csv(nchda_analysis, file = 'nhcda_processed_27102022.csv', row.names = TRUE)

###################################################################################################
## run and update primary ecmo
## subgroup of records - 1,2,4,6,7
nchda_ecmo <- subset(nchda_analysis, ((type_procedure %in% c(1,2,4,6,7) & (report_group != 'unallocated') & (report_group != 'no_valid_codes'))))

nchda_ecmo <- ecmo_allocation(nchda_ecmo)

## update nchda_analysis from nchda_ecmo
ecmo_records <- row.names(subset(nchda_ecmo, (report_group == 'primary_ecmo')))
nchda_analysis[rownames(nchda_analysis) %in% ecmo_records,'report_group'] <- 'primary_ecmo'

## set ecmo > unallocated-ecmo
nchda_analysis[,'report_group'][nchda_analysis[,'report_group'] == 'ecmo'] <- 'unallocated-ecmo'


###################################################################################################
## tabulate results & export to CSV
table(nchda_analysis[, 'report_group'])


setwd("~/CCU007_01/data")
## write finalised data to CSV

write.csv(nchda_analysis, file = 'nhcda_processed_v6.14.csv', row.names = TRUE)
###################################################################################################