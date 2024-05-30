# Creating the final anlaysis data for the CCU007_01
# Date 27 Sep 2023
# Author: Arun





# Loading packages

library(lubridate)
library(dplyr)
library(readr)
library(stringr)

library(ggplot2)

library(nnet)

# setting working directory

setwd("~/CCU007_01/data")


# removing data/objects from the r environment

rm(list = setdiff(ls(), "con"))

# inputting data
data <- read_csv("ccu007_01_data_extract.csv")

# number of cases ---------------------------------------------------------
data <- data %>% 
  dplyr::mutate(dob=as.Date(`DATE_OF_BIRTH`),
                visit_dt=as.Date(`3_01_DATE_OF_VISIT`),
                age_years=(visit_dt-dob)/365.25)
data$age_years <- as.numeric(data$age_years)

data$age_yr <- abs(round(data$age_years, digits = 3))

data <- data %>%
  dplyr::mutate(aged=age_yr*365.25,
                agegp=ifelse(aged<365.25, 1,
                              ifelse(aged>=365.25 & aged<1826.25,2,
                                     ifelse(aged>=1826.25 & aged<3652.5,3,
                                            ifelse(aged>=3652.5 & !is.na(aged),4, NA)))))


data$agegp_cat[data$agegp==1] <- "Infant"
data$agegp_cat[data$agegp==2] <- "One to <5"
data$agegp_cat[data$agegp==3] <- "Five to <10"
data$agegp_cat[data$agegp==4] <- "Ten & above"


data1 <- data %>% 
  dplyr::mutate(visit_dt=as.Date(`3_01_DATE_OF_VISIT`),
                week=week(visit_dt),
                mon= month(visit_dt),
                year=year(visit_dt),
                first_lock=as.Date('2020-03-23'),
                first_lk_wk=week(first_lock),
                first_lock_end=as.Date('2020-06-23'),
                first_unlk_wk=week(first_lock_end),
                second_lock=as.Date('2020-11-05'),
                second_lk_wk=week(second_lock),
                second_lock_end=as.Date('2020-12-02'),
                second_unlk_wk=week(second_lock_end),
                third_lock=as.Date('2021-01-06'),
                third_lk_wk=week(third_lock),
                third_unlock1=as.Date('2021-03-08'),
                third_unlock2=as.Date('2021-03-12'),
                third_unlock3=as.Date('2021-05-17'),
                third_unlock4=as.Date('2021-06-21'))

data1 <- data1 %>% 
  dplyr::mutate(child = ifelse(age_yr<16.0,1,0)) %>% 
  dplyr::filter(child==1)


data1 <- data1 %>% 
  dplyr::mutate(unselect=ifelse(year==2022 & mon>3,1,0)) %>% 
  dplyr::filter(unselect!=1) %>% 
  dplyr::select(-c(unselect)) 
  

data1$lock[data1$visit_dt<data1$first_lock] <- 1
data1$lock[data1$first_lock<=data1$visit_dt & data1$visit_dt<=data1$first_lock_end] <- 2
data1$lock[data1$first_lock_end<data1$visit_dt & data1$visit_dt<=data1$second_lock] <- 3
data1$lock[data1$second_lock<=data1$visit_dt & data1$visit_dt<=data1$second_lock_end] <- 4
data1$lock[data1$second_lock_end<data1$visit_dt & data1$visit_dt<=data1$third_lock] <- 5
data1$lock[data1$third_lock<=data1$visit_dt & data1$visit_dt<=data1$third_unlock4] <- 6
data1$lock[data1$third_unlock4<data1$visit_dt] <- 7


# Basic_description_nicor_patient_imd -------------------------------------
library(dplyr)

data1$gen <- data1$`1_07_GENDER`

data1 <- data1 %>% 
  mutate(gender=ifelse(gen=="1. Male",1,
                       ifelse(gen=="2. Female",2,
                              ifelse(gen=="2. No", 2,
                                     ifelse(gen=="9. Not specified"& SEX==1,1,
                                            ifelse(gen=="9. Not specified"& SEX==2,2,0))))))


data1$eth <- data1$`1_08_ETHNIC_GROUP`

data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="A"] <- "A. White - British"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="B"] <- "B. White - Irish"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="C"] <- "C. White - Any other White background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="D"] <- "D. Mixed - White and Black Caribbean"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="E"] <- "E. Mixed - White and Black African"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="F"] <- "F. Mixed - White and Asian"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="G"] <- "G. Mixed - Any other mixed background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="H"] <- "H. Asian - Indian"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="J"] <- "J. Asian - Pakistani"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="K"] <- "K. Asian - Bangladeshi"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="L"] <- "L. Asian - Any other Asian background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="M"] <- "M. Black - Caribbean"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="N"] <- "N. Black - African"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="P"] <- "P. Black - Any other Black background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="R"] <- "R. Other - Chinese"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="Z. Not stated" &data1$ETHNIC=="S"] <- "S. Other - Any other ethnic group"
table(data1$eth)


data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="A"] <- "A. White - British"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="B"] <- "B. White - Irish"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="C"] <- "C. White - Any other White background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="D"] <- "D. Mixed - White and Black Caribbean"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="E"] <- "E. Mixed - White and Black African"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="F"] <- "F. Mixed - White and Asian"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="G"] <- "G. Mixed - Any other mixed background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="H"] <- "H. Asian - Indian"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="J"] <- "J. Asian - Pakistani"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="K"] <- "K. Asian - Bangladeshi"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="L"] <- "L. Asian - Any other Asian background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="M"] <- "M. Black - Caribbean"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="N"] <- "N. Black - African"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="P"] <- "P. Black - Any other Black background"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="R"] <- "R. Other - Chinese"
data1$eth[data1$`1_08_ETHNIC_GROUP`=="9. Unknown" &data1$ETHNIC=="S"] <- "S. Other - Any other ethnic group"
table(data1$eth)


# shorten the ethnic group categories -------------------------------------

data1$eth_cat[data1$eth=="A. White - British"|data1$eth=="B. White - Irish"|data1$eth=="C. White - Any other White background"] <- "White"
data1$eth_cat[data1$eth=="H. Asian - Indian"|data1$eth=="J. Asian - Pakistani"|data1$eth=="K. Asian - Bangladeshi"] <- "South Asian"
data1$eth_cat[data1$eth=="M. Black - Caribbean"|data1$eth=="N. Black - African"|data1$eth=="P. Black - Any other Black background"] <- "African/Caribbean"
data1$eth_cat[data1$eth=="D. Mixed - White and Black Caribbean"|data1$eth=="E. Mixed - White and Black African"| data1$eth=="F. Mixed - White and Asian"|data1$eth=="G. Mixed - Any other mixed background"] <- "Mixed"
data1$eth_cat[data1$eth=="S. Other - Any other ethnic group"|data1$eth=="R. Other - Chinese"|data1$eth=="L. Asian - Any other Asian background"] <- "Other"
data1$eth_cat[data1$eth=="Z. Not stated"|data1$eth=="9. Unknown"] <- "Not stated/unknown"

data1 <- data1 %>% 
  dplyr::mutate(proc_hr=hour(`3_01_DATE_OF_VISIT`),
                proc_min=minute(`3_01_DATE_OF_VISIT`),
                proc_time=paste(proc_hr, proc_min, sep=":")) %>% 
  dplyr::mutate(uniq1=paste(`1_03_NHS_NUMBER_DEID`, visit_dt),
                uniq2=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time),
                uniq3=paste(`1_03_NHS_NUMBER_DEID`,visit_dt, proc_time),
                uniq4=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`),
                uniq5=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`, `4_01_DATE_OF_DISCHARGE` ))


# activity group <- "nhcda_processed_v6.14.csv"
# specific procedure group <- "sp_algorithm.csv"
# death data <- "ccu007_01_deathdata_extract.csv"
# cohort_data <- "ccu007_01_data_extract.csv"

####################Integration of death into episode data###################

setwd("~/CCU007_01/data")



death <- read_csv("ccu007_01_deathdata_extract.csv")






data1 <-  data1 %>% 
  dplyr::mutate(proc_hr=hour(`3_01_DATE_OF_VISIT`),
                proc_min=minute(`3_01_DATE_OF_VISIT`),
                proc_time=paste(proc_hr, proc_min, sep=":")) %>% 
  dplyr::mutate(uniq1=paste(`1_03_NHS_NUMBER_DEID`, visit_dt),
                uniq2=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time),
                uniq3=paste(`1_03_NHS_NUMBER_DEID`,visit_dt, proc_time),
                uniq4=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`),
                uniq5=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`, `4_01_DATE_OF_DISCHARGE` ))

data2 <- data1 %>% 
  dplyr::select("1_03_NHS_NUMBER_DEID","2_04_ANTENATAL_DIAGNOSIS","2_05_PRE_PROCEDURE_SEIZURES", 
                "3_01B_PROCEDURE_URGENCY","3_01C_UNPLANNED_REOPERATION","4_01_DATE_OF_DISCHARGE", "4_03_DISCHARGE_STATUS",
                "4_04_DISCHARGE_DESTINATION","4_09_ATTRIBUTION_OF_DEATH","6_01_PRE_PROCEDURE_NYHA_STATUS",
                "6_02_PRE_PROCEDURE_SMOKING_STATUS", "6_03_PRE_PROCEDURE_DIABETES", "8_01_PROCEDURE_UNIQUE_ID",
                "child", "age_yr","agegp", "DATE_OF_DEATH", "visit_dt", "uniq4", "proc_time") 


death <- death %>% 
  dplyr::select("1_03_NHS_NUMBER_DEID","REG_DATE_OF_DEATH_FORMATTED","REG_DATE_FORMATTED",
                "S_UNDERLYING_COD_ICD10","S_COD_CODE_1","S_COD_CODE_2","S_COD_CODE_3",
                "S_COD_CODE_4","S_COD_CODE_5","S_COD_CODE_6","S_COD_CODE_7","S_COD_CODE_8",
                "S_COD_CODE_9","S_COD_CODE_10","S_COD_CODE_11","S_COD_CODE_12","S_COD_CODE_13",
                "S_COD_CODE_14","S_COD_CODE_15","NEO_NATE_FLAG")


death <- death %>% 
  dplyr::mutate(i_le_undr=substr(S_UNDERLYING_COD_ICD10,1,1),
                i_num_undr=substr(S_UNDERLYING_COD_ICD10,2,4),
                i_le_cod1=substr(S_COD_CODE_1,1,1),
                i_num_cod1=substr(S_COD_CODE_1,2,4),
                i_le_cod2=substr(S_COD_CODE_2,1,1),
                i_num_cod2=substr(S_COD_CODE_2,2,4)) %>% 
  dplyr::mutate(i_num_undr=as.numeric(i_num_undr),
                i_num_cod1=as.numeric(i_num_cod1),
                i_num_cod2=as.numeric(i_num_cod2))

#underlying_cod
death <- death %>% 
  dplyr::mutate(cod_cat=ifelse(i_le_undr=="A"|i_le_undr=="B", "infections",
                               ifelse(i_le_undr=="C", "neoplasm", 
                                      ifelse(i_le_undr=="D" & i_num_undr<500, "neoplasm",
                                             ifelse(i_le_undr=="D"& i_num_undr>499, "blood",
                                                    ifelse(i_le_undr=="E", "endocrine",
                                                           ifelse(i_le_undr=="F", "mental",
                                                                  ifelse(i_le_undr=="G", "nervous",
                                                                         ifelse(i_le_undr=="H" &i_num_undr<600, "eye",
                                                                                ifelse(i_le_undr=="H" & i_num_undr>599, "ear",
                                                                                       ifelse(i_le_undr=="I", "circulatory",
                                                                                              ifelse(i_le_undr=="J", "respiratory",
                                                                                                     ifelse(i_le_undr=="K", "digestive",
                                                                                                            ifelse(i_le_undr=="L", "skin",
                                                                                                                   ifelse(i_le_undr=="M", "muscles",
                                                                                                                          ifelse(i_le_undr=="N", "genitourinary",
                                                                                                                                 ifelse(i_le_undr=="O", "childbirth",
                                                                                                                                        ifelse(i_le_undr=="P", "perinatal",
                                                                                                                                               ifelse(i_le_undr=="Q", "congenital",
                                                                                                                                                      ifelse(i_le_undr=="R", "NEC",
                                                                                                                                                             ifelse(i_le_undr=="S"|i_le_undr=="T", "injury",
                                                                                                                                                                    ifelse(i_le_undr=="V"|i_le_undr=="W"|i_le_undr=="X"|i_le_undr=="Y", "external",
                                                                                                                                                                           ifelse(i_le_undr=="Z", "health", "special")))))))))))))))))))))))

#cod1_underlying
death <- death %>% 
  dplyr::mutate(cod1_cat=ifelse(i_le_cod1=="A"|i_le_cod1=="B", "infections",
                                ifelse(i_le_cod1=="C", "neoplasm", 
                                       ifelse(i_le_cod1=="D" & i_num_cod1<500, "neoplasm",
                                              ifelse(i_le_cod1=="D"& i_num_cod1>499, "blood",
                                                     ifelse(i_le_cod1=="E", "endocrine",
                                                            ifelse(i_le_cod1=="F", "mental",
                                                                   ifelse(i_le_cod1=="G", "nervous",
                                                                          ifelse(i_le_cod1=="H" &i_num_cod1<600, "eye",
                                                                                 ifelse(i_le_cod1=="H" & i_num_cod1>599, "ear",
                                                                                        ifelse(i_le_cod1=="I", "circulatory",
                                                                                               ifelse(i_le_cod1=="J", "respiratory",
                                                                                                      ifelse(i_le_cod1=="K", "digestive",
                                                                                                             ifelse(i_le_cod1=="L", "skin",
                                                                                                                    ifelse(i_le_cod1=="M", "muscles",
                                                                                                                           ifelse(i_le_cod1=="N", "genitourinary",
                                                                                                                                  ifelse(i_le_cod1=="O", "childbirth",
                                                                                                                                         ifelse(i_le_cod1=="P", "perinatal",
                                                                                                                                                ifelse(i_le_cod1=="Q", "congenital",
                                                                                                                                                       ifelse(i_le_cod1=="R", "NEC",
                                                                                                                                                              ifelse(i_le_cod1=="S"|i_le_cod1=="T", "injury",
                                                                                                                                                                     ifelse(i_le_cod1=="V"|i_le_cod1=="W"|i_le_cod1=="X"|i_le_cod1=="Y", "external",
                                                                                                                                                                            ifelse(i_le_cod1=="Z", "health", "special")))))))))))))))))))))))

#cod2_underlying


death <- death %>% 
  dplyr::mutate(cod2_cat=ifelse(i_le_cod2=="A"|i_le_cod2=="B", "infections",
                                ifelse(i_le_cod2=="C", "neoplasm", 
                                       ifelse(i_le_cod2=="D" & i_num_cod2<500, "neoplasm",
                                              ifelse(i_le_cod2=="D"& i_num_cod2>499, "blood",
                                                     ifelse(i_le_cod2=="E", "endocrine",
                                                            ifelse(i_le_cod2=="F", "mental",
                                                                   ifelse(i_le_cod2=="G", "nervous",
                                                                          ifelse(i_le_cod2=="H" &i_num_cod2<600, "eye",
                                                                                 ifelse(i_le_cod2=="H" & i_num_cod2>599, "ear",
                                                                                        ifelse(i_le_cod2=="I", "circulatory",
                                                                                               ifelse(i_le_cod2=="J", "respiratory",
                                                                                                      ifelse(i_le_cod2=="K", "digestive",
                                                                                                             ifelse(i_le_cod2=="L", "skin",
                                                                                                                    ifelse(i_le_cod2=="M", "muscles",
                                                                                                                           ifelse(i_le_cod2=="N", "genitourinary",
                                                                                                                                  ifelse(i_le_cod2=="O", "childbirth",
                                                                                                                                         ifelse(i_le_cod2=="P", "perinatal",
                                                                                                                                                ifelse(i_le_cod2=="Q", "congenital",
                                                                                                                                                       ifelse(i_le_cod2=="R", "NEC",
                                                                                                                                                              ifelse(i_le_cod2=="S"|i_le_cod2=="T", "injury",
                                                                                                                                                                     ifelse(i_le_cod2=="V"|i_le_cod2=="W"|i_le_cod2=="X"|i_le_cod2=="Y", "external",
                                                                                                                                                                            ifelse(i_le_cod2=="Z", "health", "special")))))))))))))))))))))))




death <- death %>% 
  dplyr::select("1_03_NHS_NUMBER_DEID","REG_DATE_OF_DEATH_FORMATTED" ,"cod_cat", "cod1_cat", "cod2_cat" ) %>% 
  dplyr::mutate(death=1)


final_death <- merge(data2, death, by.x="1_03_NHS_NUMBER_DEID", by.y = "1_03_NHS_NUMBER_DEID" , all.x = "TRUE")
final_death <- final_death %>% 
  dplyr::mutate(related = REG_DATE_OF_DEATH_FORMATTED-`4_01_DATE_OF_DISCHARGE`) %>% 
  dplyr::mutate(related_cat= ifelse(related<30, 1, 0))
death_sp_final <- final_death %>% 
  group_by(`1_03_NHS_NUMBER_DEID`) %>% 
  arrange(visit_dt, proc_time, `8_01_PROCEDURE_UNIQUE_ID`) %>% 
  dplyr::mutate(num=row_number(),
                max=max(num)) %>% 
  ungroup() %>% 
  dplyr::mutate(death_cat=ifelse(num==max,death,NA)) 

write.csv(death_sp_final, "ccu007_01_death_final.csv" )


#################integration of death complete###################

# activity group <- "nhcda_processed_v6.14.csv"
# specific procedure group <- "sp_algorithm.csv"
# death data <- "ccu007_01_deathdata_extract.csv"
# cohort_data <- "ccu007_01_data_extract.csv"
# death_final <- "ccu007_01_death_final.csv"
###################################################################


# Diagnosis workup --------------------------------------------------------


library(tidyr)



data_new <- data1 %>%
  dplyr::select(`1_03_NHS_NUMBER_DEID`,`2_01_DIAGNOSIS1`, visit_dt, uniq4)


# NCHDA diagnosis group mapping ST-------------------------------------------
library(readr)
rm(list = setdiff(ls(), c("con", "data",   "data1","data_new")))
mmc1 <- read_csv("mmc1.csv")
library(dplyr)


mmc2 <- mmc1 %>%
  dplyr::mutate(nchda_code=`nchda code`,
                diag_gp=`diagnosis group`,
                diag_rk=`diagnosis group rank`,
                prais_rk=`PRAiS2 group rank`,
                uvh=UVH) %>% 
  dplyr::select("nchda_code", "diag_gp","diag_rk", "prais_rk", "uvh")

mmc2 <- mmc2 %>% 
  separate(nchda_code, c("code", "nchda_diag"), extra = "merge", fill= "right")




x<- data_new %>% 
  separate(`2_01_DIAGNOSIS1`, c("diag1", "diag2", "diag3", "diag4", "diag5", "diag6","diag7", "diag8", "diag9", "diag10" ), ";", extra="merge", fill = "right")



data_dgcode<- x %>% 
  
  separate(diag1, c("dcode1", "desc1"), extra = "merge", fill ="right") %>%
  separate(diag2, c("dcode2", "desc2"), extra = "merge", fill ="right") %>%
  separate(diag3, c("dcode3", "desc3"), extra = "merge", fill ="right") %>%
  separate(diag4, c("dcode4", "desc4"), extra = "merge", fill ="right") %>% 
  separate(diag5, c("dcode5", "desc5"), extra = "merge", fill ="right") %>% 
  separate(diag6, c("dcode6", "desc6"), extra = "merge", fill ="right") %>% 
  separate(diag7, c("dcode7", "desc7"), extra = "merge", fill ="right") %>% 
  separate(diag8, c("dcode8", "desc8"), extra = "merge", fill ="right") %>% 
  separate(diag9, c("dcode9", "desc9"), extra = "merge", fill ="right") %>% 
  separate(diag10, c("dcode10", "desc10"), extra = "merge", fill ="right")

names(data_dgcode)


data_dgcode$dcode1[(nchar(data_dgcode$dcode1))<6] <- NA
data_dgcode$dcode2[(nchar(data_dgcode$dcode2))<6] <- NA
data_dgcode$dcode3[(nchar(data_dgcode$dcode3))<6] <- NA
data_dgcode$dcode4[(nchar(data_dgcode$dcode4))<6] <- NA
data_dgcode$dcode5[(nchar(data_dgcode$dcode5))<6] <- NA
data_dgcode$dcode6[(nchar(data_dgcode$dcode6))<6] <- NA
data_dgcode$dcode7[(nchar(data_dgcode$dcode7))<6] <- NA
data_dgcode$dcode8[(nchar(data_dgcode$dcode8))<6] <- NA
data_dgcode$dcode9[(nchar(data_dgcode$dcode9))<6] <- NA
data_dgcode$dcode10[(nchar(data_dgcode$dcode10))<6] <- NA

summary(nchar(data_dgcode$dcode1))
summary(nchar(data_dgcode$dcode2))
summary(nchar(data_dgcode$dcode3))
summary(nchar(data_dgcode$dcode4))
summary(nchar(data_dgcode$dcode5))
summary(nchar(data_dgcode$dcode6))
summary(nchar(data_dgcode$dcode7))
summary(nchar(data_dgcode$dcode8))
summary(nchar(data_dgcode$dcode9))
summary(nchar(data_dgcode$dcode10))



# merging the codes with nicor --------------------------------------------


data_dcode <- data_dgcode %>% 
  dplyr::select("1_03_NHS_NUMBER_DEID", "visit_dt", "dcode1", "dcode2", "dcode3",
                "dcode4", "dcode5", "dcode6", "dcode7", "dcode8", "dcode9", "dcode10", "uniq4")


data_dcode1 <- merge(data_dcode, mmc2, by.x = 'dcode1', by.y = 'code', all.x = TRUE)


data_dcode1 <- data_dcode1 %>% 
  dplyr::mutate(nchda_diag1=nchda_diag,
                diag_gp1=diag_gp,
                diag_rk1=diag_rk,
                uvh1=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1", "uniq4")




data_dcode2 <- merge(data_dcode1, mmc2, by.x = 'dcode2', by.y = 'code', all.x = TRUE)


data_dcode2 <- data_dcode2 %>% 
  dplyr::mutate(nchda_diag2=nchda_diag,
                diag_gp2=diag_gp,
                diag_rk2=diag_rk,
                uvh2=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2", "uniq4")



data_dcode3 <- merge(data_dcode2, mmc2, by.x = 'dcode3', by.y = 'code', all.x = TRUE)


data_dcode3 <- data_dcode3 %>% 
  dplyr::mutate(nchda_diag3=nchda_diag,
                diag_gp3=diag_gp,
                diag_rk3=diag_rk,
                uvh3=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3", "uniq4")




data_dcode4 <- merge(data_dcode3, mmc2, by.x = 'dcode4', by.y = 'code', all.x = TRUE)


data_dcode4 <- data_dcode4 %>% 
  dplyr::mutate(nchda_diag4=nchda_diag,
                diag_gp4=diag_gp,
                diag_rk4=diag_rk,
                uvh4=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4", "uniq4")



data_dcode5 <- merge(data_dcode4, mmc2, by.x = 'dcode5', by.y = 'code', all.x = TRUE)


data_dcode5 <- data_dcode5 %>% 
  dplyr::mutate(nchda_diag5=nchda_diag,
                diag_gp5=diag_gp,
                diag_rk5=diag_rk,
                uvh5=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4",
                 "nchda_diag5","diag_gp5","diag_rk5","uvh5", "uniq4")



data_dcode6 <- merge(data_dcode5, mmc2, by.x = 'dcode6', by.y = 'code', all.x = TRUE)


data_dcode6 <- data_dcode6 %>% 
  dplyr::mutate(nchda_diag6=nchda_diag,
                diag_gp6=diag_gp,
                diag_rk6=diag_rk,
                uvh6=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4",
                 "nchda_diag5","diag_gp5","diag_rk5","uvh5",
                 "nchda_diag6","diag_gp6","diag_rk6","uvh6", "uniq4")



data_dcode7 <- merge(data_dcode6, mmc2, by.x = 'dcode7', by.y = 'code', all.x = TRUE)


data_dcode7 <- data_dcode7 %>% 
  dplyr::mutate(nchda_diag7=nchda_diag,
                diag_gp7=diag_gp,
                diag_rk7=diag_rk,
                uvh7=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4",
                 "nchda_diag5","diag_gp5","diag_rk5","uvh5",
                 "nchda_diag6","diag_gp6","diag_rk6","uvh6",
                 "nchda_diag7","diag_gp7","diag_rk7","uvh7", "uniq4")



data_dcode8 <- merge(data_dcode7, mmc2, by.x = 'dcode8', by.y = 'code', all.x = TRUE)


data_dcode8 <- data_dcode8 %>% 
  dplyr::mutate(nchda_diag8=nchda_diag,
                diag_gp8=diag_gp,
                diag_rk8=diag_rk,
                uvh8=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4",
                 "nchda_diag5","diag_gp5","diag_rk5","uvh5",
                 "nchda_diag6","diag_gp6","diag_rk6","uvh6",
                 "nchda_diag7","diag_gp7","diag_rk7","uvh7",
                 "nchda_diag8","diag_gp8","diag_rk8","uvh8", "uniq4")



data_dcode9 <- merge(data_dcode8, mmc2, by.x = 'dcode9', by.y = 'code', all.x = TRUE)


data_dcode9 <- data_dcode9 %>% 
  dplyr::mutate(nchda_diag9=nchda_diag,
                diag_gp9=diag_gp,
                diag_rk9=diag_rk,
                uvh9=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4",
                 "nchda_diag5","diag_gp5","diag_rk5","uvh5",
                 "nchda_diag6","diag_gp6","diag_rk6","uvh6",
                 "nchda_diag7","diag_gp7","diag_rk7","uvh7",
                 "nchda_diag8","diag_gp8","diag_rk8","uvh8",
                 "nchda_diag9","diag_gp9","diag_rk9","uvh9", "uniq4")




data_dcode10 <- merge(data_dcode9, mmc2, by.x = 'dcode10', by.y = 'code', all.x = TRUE)


data_dcode10 <- data_dcode10 %>% 
  dplyr::mutate(nchda_diag10=nchda_diag,
                diag_gp10=diag_gp,
                diag_rk10=diag_rk,
                uvh10=uvh) %>% 
  dplyr::select( "dcode1","1_03_NHS_NUMBER_DEID","visit_dt","dcode2","dcode3",
                 "dcode4","dcode5","dcode6","dcode7","dcode8","dcode9","dcode10",
                 "nchda_diag1","diag_gp1","diag_rk1","uvh1",
                 "nchda_diag2","diag_gp2","diag_rk2","uvh2",
                 "nchda_diag3","diag_gp3","diag_rk3","uvh3",
                 "nchda_diag4","diag_gp4","diag_rk4","uvh4",
                 "nchda_diag5","diag_gp5","diag_rk5","uvh5",
                 "nchda_diag6","diag_gp6","diag_rk6","uvh6",
                 "nchda_diag7","diag_gp7","diag_rk7","uvh7",
                 "nchda_diag8","diag_gp8","diag_rk8","uvh8",
                 "nchda_diag9","diag_gp9","diag_rk9","uvh9",
                 "nchda_diag10","diag_gp10","diag_rk10","uvh10", "uniq4")



library(matrixStats)
data_dcodefinal <- data_dcode10 %>% 
  rowwise() %>% 
  dplyr::mutate(diag_rank=min(c(diag_rk1, diag_rk2, diag_rk3, diag_rk4, diag_rk5, diag_rk6, diag_rk7, diag_rk8, diag_rk9, diag_rk10), na.rm=TRUE),
                diag_rank6=min(c(diag_rk1, diag_rk2, diag_rk3, diag_rk4, diag_rk5, diag_rk6), na.rm=TRUE))




###########################################

mmc_dg <- mmc1 %>% 
  group_by(`diagnosis group`) %>% 
  dplyr::summarise(rank_dg=mean(`diagnosis group rank`))

dcode_final <- merge(data_dcodefinal, mmc_dg, by.x = "diag_rank6", by.y="rank_dg", all.x = TRUE)

rm(list = setdiff(ls(), c("con", "data",   "data1","mmc2","dcode_final",  "mmc1")))

mmc3 <- mmc2 %>% 
  dplyr::group_by(diag_rk) %>% 
  dplyr::mutate(rown=row_number()) %>% 
  dplyr::filter(rown==1) %>% 
  dplyr::select(diag_rk, prais_rk)

dcode_final <- merge(dcode_final,mmc3, by.x="diag_rank6", by.y="diag_rk", all.x=TRUE )


setwd("~/CCU007_01/data")
write.csv(dcode_final, "ccu007_01_diagnosis.csv" )


#####################end of diagnosis work up################

# activity group <- "nhcda_processed_v6.14.csv"
# specific procedure group <- "sp_algorithm.csv"
# death data <- "ccu007_01_deathdata_extract.csv"
# cohort_data <- "ccu007_01_data_extract.csv"
# death_final <- "ccu007_01_death_final.csv"
# diagnosis_final <- "ccu007_01_diagnosis.csv"

################################################################



###################PRAiS2 model#################################
library(lubridate)
library(dplyr)
library(readr)
library(tidyr) # for separate function
#install.packages("ggsci")
library(ggsci)

setwd("~/CCU007_01/scripts")

spc_code <- read_csv("spc_code.csv")

rm(list = setdiff(ls(), c("con", "data1","spc_code")))
# selecting variables to go forward for PRAIS2 model ----------------------




d1 <- data1 %>% 
  dplyr::select("NHS_NUMBER_DEID","3_01_DATE_OF_VISIT", "DATE_OF_BIRTH", "dob", "age_yr",
                "2_03_WEIGHT", "2_03B_HEIGHT","2_01_DIAGNOSIS1","proc_time","visit_dt","uniq4",
                "3_07_TYPE_OF_PROCEDURE", "2_07_COMORBID_CONDITIONS")



d2 <- read_csv("sp_algorithm.csv")


d2 <- d2 %>% 
  dplyr::rename("NHS_NUMBER_DEID"="1_03_NHS_NUMBER_DEID",
                "type_procedure"="type_procedure.y") %>% 
  dplyr::select("NHS_NUMBER_DEID", "uniq4", "sp_algorithm_group", "type_procedure")

d3 <- read.csv("nhcda_processed.csv")

d3 <- d3 %>% 
  dplyr::select("patient_identifier", "uniq4", "report_group")
d3 <- d3 %>% 
  dplyr::select("patient_identifier", "uniq4", "report_group") %>% 
  dplyr::mutate(report_num=ifelse(report_group=="bypass",1,
                                  ifelse(report_group=="diagnostic:non-surgical",2,
                                         ifelse(report_group=="ep:non-surgical",3,
                                                ifelse(report_group=="hybrid",4,
                                                       ifelse(report_group=="icd:non-surgical",5,
                                                              ifelse(report_group=="intervention:non-surgical",6,
                                                                     ifelse(report_group=="no_qualifying_codes", 7, 
                                                                            ifelse(report_group=="non-bypass",8,
                                                                                   ifelse(report_group=="pacemaker:non-surgical", 9,
                                                                                          ifelse(report_group=="primary_ecmo",10,
                                                                                                 ifelse(report_group=="unallocated",11,
                                                                                                        ifelse(report_group=="unallocated-ecmo",12,
                                                                                                               13)))))))))))))

d3 <- d3 %>% 
  dplyr::mutate(actv_cat=ifelse(report_num==1|report_num==4|report_num==8,1,
                                ifelse(report_num==6,2,
                                       ifelse(report_num==2,3,
                                              ifelse(report_num==3|report_num==5|report_num==9,4,
                                                     ifelse(report_num==7, 5,6))))),
                actv_cat_name=ifelse(actv_cat==1, "Surgery",
                                     ifelse(actv_cat==2, "Intv_cath",
                                            ifelse(actv_cat==3, "Diag_cath",
                                                   ifelse(actv_cat==4, "Ep",
                                                          ifelse(actv_cat==5, "Chest_cl_expl",
                                                                 "Mechanical_support"))))))


d4 <-  read.csv("ccu007_01_diagnosis.csv")

d4 <- d4 %>% 
  dplyr::rename("NHS_NUMBER_DEID"="X1_03_NHS_NUMBER_DEID",
                "prais_rank6"="prais_rk") %>% 
  dplyr::select("uniq4", "diag_rank6", "diag_rank", "prais_rank6", "NHS_NUMBER_DEID",
                "uvh1", "uvh2","uvh3", "uvh4","uvh5", "uvh6","uvh7", "uvh8","uvh9", "uvh10")
library(tidyr)
d2 <- d2 %>% 
  separate(sp_algorithm_group, c("spg_code", "spg_group" ), ":", extra="merge", fill = "right")

spc_code <- spc_code %>% 
  separate(sp_code,c("spc","desc"), " ",extra="merge", fill = "right" )


spc_code1 <- spc_code[!duplicated(spc_code$spc),]

d12 <- merge(d1, d2, by.x = "uniq4", by.y = "uniq4", all.x = TRUE)

d123 <- merge(d12, d3, by.x = "uniq4", by.y = "uniq4", all.x = TRUE)

d1234 <- merge(d123, d4, by.x = "uniq4", by.y = "uniq4", all.x = TRUE)

data2<- merge(d1234, spc_code1, by.x = 'spg_code', by.y = 'spc', all.x = TRUE)


# limiting the data to cardiac surgical procedures
rm("d1")
rm("d2")
rm("d3")
rm("d4")
rm("d12")
rm("d123")
rm("d1234")
rm("spc_code")

data2 <- data2 %>% 
  dplyr::mutate(sp_group=ifelse(diag_rank6==1 &  actv_cat==1 & report_num==4, 1, sp_group))

data3 <-  data2 %>% 
  dplyr::filter(actv_cat==1)

table(data3$spg_group[is.na(data3$sp_group)])

# missing 'sp_group' are declared as 'no specific' procedure (20)
data3$sp_group[is.na(data3$sp_group)] <- 20
table(data3$sp_group)

data3 <- data3 %>% 
  dplyr::mutate(uvh_final=ifelse(uvh1=="Yes"|uvh2=="Yes"|uvh3=="Yes"|uvh4=="Yes"|uvh5=="Yes"|uvh6=="Yes"|uvh7=="Yes"|uvh8=="Yes"|uvh9=="Yes"|uvh10=="Yes",1,0))

data3$uvh_final[is.na(data3$uvh_final)] <- 0


table(data3$uvh_final)

ls(data3)
data3 <- data3 %>% 
  dplyr::select( "NHS_NUMBER_DEID", "3_01_DATE_OF_VISIT","visit_dt", "DATE_OF_BIRTH","dob",
                 "age_yr","2_03_WEIGHT","2_03B_HEIGHT", "spg_group","report_group" ,"type_procedure"
                 ,"3_07_TYPE_OF_PROCEDURE", "2_07_COMORBID_CONDITIONS","proc_time","Procedure Title",
                 "Type","sp_group","diag_rank","diag_rank6","prais_rank6","uvh_final", "uniq4")


data3 <- data3 %>%
  dplyr::mutate(com=`2_07_COMORBID_CONDITIONS`) %>% 
  separate(`2_07_COMORBID_CONDITIONS`, c("comorb1", "comorb2", "comorb3", "comorb4", "comorb5", "comorb6","comorb7","comorb8" ), ";", extra="merge", fill = "right")


data3 <- data3 %>% 
  separate(comorb1, c("morb1", "desc1"), extra = "merge", fill ="right") %>%
  separate(comorb2, c("morb2", "desc2"), extra = "merge", fill ="right") %>%
  separate(comorb3, c("morb3", "desc3"), extra = "merge", fill ="right") %>%
  separate(comorb4, c("morb4", "desc4"), extra = "merge", fill ="right") %>%
  separate(comorb5, c("morb5", "desc5"), extra = "merge", fill ="right") %>%
  separate(comorb6, c("morb6", "desc6"), extra = "merge", fill ="right") %>%
  separate(comorb7, c("morb7", "desc7"), extra = "merge", fill ="right") %>%
  separate(comorb8, c("morb8", "desc8"), extra = "merge", fill ="right") 


data3$morb1[(nchar(data3$morb1))<6] <- NA
data3$morb2[(nchar(data3$morb2))<6] <- NA
data3$morb3[(nchar(data3$morb3))<6] <- NA
data3$morb4[(nchar(data3$morb4))<6] <- NA
data3$morb5[(nchar(data3$morb5))<6] <- NA
data3$morb6[(nchar(data3$morb6))<6] <- NA
data3$morb7[(nchar(data3$morb7))<6] <- NA
data3$morb8[(nchar(data3$morb8))<6] <- NA


summary(nchar(data3$morb1))
summary(nchar(data3$morb2))
summary(nchar(data3$morb3))
summary(nchar(data3$morb4))
summary(nchar(data3$morb5))
summary(nchar(data3$morb6))
summary(nchar(data3$morb7))
summary(nchar(data3$morb8))

cmorb_code <- read_csv("cmorb_code.csv")


cmorb_code <- cmorb_code %>% 
  separate(cmorb, c("code", "desc"), extra = "merge", fill ="right")


dmorb1 <- merge(data3, cmorb_code, by.x = 'morb1', by.y = 'code', all.x = TRUE)


dmorb1 <- dmorb1 %>% 
  dplyr::mutate(risk_gp1=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

dmorb2 <- merge(dmorb1, cmorb_code, by.x = 'morb2', by.y = 'code', all.x = TRUE)
dmorb2 <- dmorb2 %>% 
  dplyr::mutate(risk_gp2=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

dmorb3 <- merge(dmorb2, cmorb_code, by.x = 'morb3', by.y = 'code', all.x = TRUE)
dmorb3 <- dmorb3 %>% 
  dplyr::mutate(risk_gp3=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))


rm("dmorb1")
rm("dmorb2")
dmorb4 <- merge(dmorb3, cmorb_code, by.x = 'morb4', by.y = 'code', all.x = TRUE)
dmorb4 <- dmorb4 %>% 
  dplyr::mutate(risk_gp4=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("dmorb3")
dmorb5 <- merge(dmorb4, cmorb_code, by.x = 'morb5', by.y = 'code', all.x = TRUE)
dmorb5 <- dmorb5 %>% 
  dplyr::mutate(risk_gp5=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("dmorb4")
dmorb6 <- merge(dmorb5, cmorb_code, by.x = 'morb6', by.y = 'code', all.x = TRUE)
dmorb6 <- dmorb6 %>% 
  dplyr::mutate(risk_gp6=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("dmorb5")
dmorb7 <- merge(dmorb6, cmorb_code, by.x = 'morb7', by.y = 'code', all.x = TRUE)
dmorb7 <- dmorb7 %>% 
  dplyr::mutate(risk_gp7=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("dmorb6")
dmorb8 <- merge(dmorb7, cmorb_code, by.x = 'morb8', by.y = 'code', all.x = TRUE)
dmorb8 <- dmorb8 %>% 
  dplyr::mutate(risk_gp8=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("dmorb7")


data_prais2 <- dmorb8 %>% 
  dplyr::mutate(acq_com=ifelse(risk_gp1=="Acquired Comorbidity"|risk_gp2=="Acquired Comorbidity"|risk_gp3=="Acquired Comorbidity"|risk_gp4=="Acquired Comorbidity"|risk_gp5=="Acquired Comorbidity"|risk_gp6=="Acquired Comorbidity"|risk_gp7=="Acquired Comorbidity"|risk_gp8=="Acquired Comorbidity", 1,0),
                acrf_com=ifelse(risk_gp1=="Additional Cardiac Risk Factors"|risk_gp2=="Additional Cardiac Risk Factors"|risk_gp3=="Additional Cardiac Risk Factors"|risk_gp4=="Additional Cardiac Risk Factors"|risk_gp5=="Additional Cardiac Risk Factors"|risk_gp6=="Additional Cardiac Risk Factors"|risk_gp7=="Additional Cardiac Risk Factors"|risk_gp8=="Additional Cardiac Risk Factors", 1,0),
                cong_com=ifelse(risk_gp1=="Congenital Comorbidity"|risk_gp2=="Congenital Comorbidity"|risk_gp3=="Congenital Comorbidity"|risk_gp4=="Congenital Comorbidity"|risk_gp5=="Congenital Comorbidity"|risk_gp6=="Congenital Comorbidity"|risk_gp7=="Congenital Comorbidity"|risk_gp8=="Congenital Comorbidity", 1,0),
                sev_ill_com=ifelse(risk_gp1=="Severity of illness"|risk_gp2=="Severity of illness"|risk_gp3=="Severity of illness"|risk_gp4=="Severity of illness"|risk_gp5=="Severity of illness"|risk_gp6=="Severity of illness"|risk_gp7=="Severity of illness"|risk_gp8=="Severity of illness",1,0))
rm("dmorb8")


data_prais2$acq_com[is.na(data_prais2$acq_com)] <- 0
data_prais2$acrf_com[is.na(data_prais2$acrf_com)] <- 0
data_prais2$cong_com[is.na(data_prais2$cong_com)] <- 0
data_prais2$sev_ill_com[is.na(data_prais2$sev_ill_com)] <- 0



table(data_prais2$sp_group)



data_prais2 <- data_prais2 %>% 
  dplyr::select(-c("morb1","morb2","morb3","morb4","morb5","morb6","morb7","morb8","desc1","desc2","desc3","desc4","desc5","desc6","desc7","desc8",
                   "risk_gp1","risk_gp2","risk_gp3","risk_gp4","risk_gp5","risk_gp6","risk_gp7","risk_gp8"))

data_prais2$age_years <- as.numeric(data_prais2$age_yr)

data_prais2 <- data_prais2 %>% 
  dplyr::mutate(diag1=ifelse(prais_rank6==1,1,0),
                diag2=ifelse(prais_rank6==2,1,0),
                diag3=ifelse(prais_rank6==3,1,0),
                diag4=ifelse(prais_rank6==4,1,0),
                diag5=ifelse(prais_rank6==5,1,0),
                diag6=ifelse(prais_rank6==6,1,0),
                diag7=ifelse(prais_rank6==7,1,0),
                diag8=ifelse(prais_rank6==8,1,0),
                diag9=ifelse(prais_rank6==9,1,0),
                diag10=ifelse(prais_rank6==10,1,0),
                diag11=ifelse(prais_rank6==11,1,0),
                sp_gp1=ifelse(sp_group==1,1,0),
                sp_gp2=ifelse(sp_group==2,1,0),
                sp_gp3=ifelse(sp_group==3,1,0),
                sp_gp4=ifelse(sp_group==4,1,0),
                sp_gp5=ifelse(sp_group==5,1,0),
                sp_gp6=ifelse(sp_group==6,1,0),
                sp_gp7=ifelse(sp_group==7,1,0),
                sp_gp8=ifelse(sp_group==8,1,0),
                sp_gp9=ifelse(sp_group==9,1,0),
                sp_gp10=ifelse(sp_group==10,1,0),
                sp_gp11=ifelse(sp_group==11,1,0),
                sp_gp12=ifelse(sp_group==12,1,0),
                sp_gp13=ifelse(sp_group==13,1,0),
                sp_gp14=ifelse(sp_group==14,1,0),
                sp_gp15=ifelse(sp_group==15,1,0),
                sp_gp20=ifelse(sp_group==20,1,0),
                bypass=ifelse(type_procedure==1,1,0)
  )


data_prais2 <- data_prais2 %>% 
  dplyr::mutate(post_2013=1,
                age_sqrt=sqrt(age_years),
                wt_sqrt=sqrt(`2_03_WEIGHT`))


#generate 'z'
data_prais2 %>%
  dplyr::mutate(z = 0.336*age_years -1.808*wt_sqrt +0.088*`2_03_WEIGHT`+0.000*diag1-0.168*diag2-0.330*diag3 - 1.521*diag4 -0512*diag5 -0.117*diag6 -0.054*diag7 - 0.631*diag8 -0.468 *diag9 -1.698*diag10 -1.241*diag11+0.000*sp_gp1 +0.216*sp_gp2 +0.625*sp_gp3 -0.090*sp_gp4+ 0.056*sp_gp5 -0.747*sp_gp6+1.066*sp_gp7+0.788*sp_gp8+1.1*sp_gp9 - 0.787*sp_gp10 -0.964*sp_gp11 -0.202*sp_gp12 -0.067*sp_gp13 -0.937*sp_gp14 -1.637*sp_gp15 +0.428*sp_gp20+0.398*bypass +0.692*uvh_final+0.731*acrf_com+0.538*acq_com+0.325*cong_com+0.689*sev_ill_com-0.28*post_2013-0.229-0.439*age_sqrt ) 

data_prais2$z <- 0.336*data_prais2$age_years -1.808*data_prais2$wt_sqrt +0.088*data_prais2$`2_03_WEIGHT`+0.000*data_prais2$diag1-0.168*data_prais2$diag2-0.330*data_prais2$diag3 - 1.521*data_prais2$diag4 -0512*data_prais2$diag5 -0.117*data_prais2$diag6 -0.054*data_prais2$diag7 - 0.631*data_prais2$diag8 -0.468 *data_prais2$diag9 -1.698*data_prais2$diag10 -1.241*data_prais2$diag11+0.000*data_prais2$sp_gp1 +0.216*data_prais2$sp_gp2 +0.625*data_prais2$sp_gp3 -0.090*data_prais2$sp_gp4+ 0.056*data_prais2$sp_gp5 -0.747*data_prais2$sp_gp6+1.066*data_prais2$sp_gp7+0.788*data_prais2$sp_gp8+1.1*data_prais2$sp_gp9 - 0.787*data_prais2$sp_gp10 -0.964*data_prais2$sp_gp11 -0.202*data_prais2$sp_gp12 -0.067*data_prais2$sp_gp13 -0.937*data_prais2$sp_gp14 -1.637*data_prais2$sp_gp15 +0.428*data_prais2$sp_gp20+0.398*data_prais2$bypass +0.692*data_prais2$uvh_final+0.731*data_prais2$acrf_com+0.538*data_prais2$acq_com+0.325*data_prais2$cong_com+0.689*data_prais2$sev_ill_com-0.28*data_prais2$post_2013-0.229-0.439*data_prais2$age_sqrt 


prais2 <- data_prais2 %>%
  dplyr::mutate(z = 0.336*age_years -1.808*wt_sqrt +0.088*`2_03_WEIGHT`+0.000*diag1-0.168*diag2-0.330*diag3 - 1.521*diag4 -0512*diag5 -0.117*diag6 -0.054*diag7 - 0.631*diag8 -0.468 *diag9 -1.698*diag10 -1.241*diag11+0.000*sp_gp1 +0.216*sp_gp2 +0.625*sp_gp3 -0.090*sp_gp4+ 0.056*sp_gp5 -0.747*sp_gp6+1.066*sp_gp7+0.788*sp_gp8+1.1*sp_gp9 - 0.787*sp_gp10 -0.964*sp_gp11 -0.202*sp_gp12 -0.067*sp_gp13 -0.937*sp_gp14 -1.637*sp_gp15 +0.428*sp_gp20+0.398*bypass +0.692*uvh_final+0.731*acrf_com+0.538*acq_com+0.325*cong_com+0.689*sev_ill_com-0.28*post_2013-0.439*age_sqrt -0.229 )
rm("data_prais2")
summary(prais2$z)
prais2 <- prais2 %>% 
  dplyr::mutate(deno=1+exp(-z),
                prais2=1/deno)



prais2 <- prais2 %>% 
  dplyr::mutate(week=week(visit_dt),
                mon= month(visit_dt),
                year=year(visit_dt),
                first_lock=as.Date('2020-03-23'),
                first_lk_wk=week(first_lock),
                first_lock_end=as.Date('2020-06-23'),
                first_unlk_wk=week(first_lock_end),
                second_lock=as.Date('2020-11-05'),
                second_lk_wk=week(second_lock),
                second_lock_end=as.Date('2020-12-02'),
                second_unlk_wk=week(second_lock_end),
                third_lock=as.Date('2021-01-06'),
                third_lk_wk=week(third_lock),
                third_unlock1=as.Date('2021-03-08'),
                third_unlock2=as.Date('2021-03-12'),
                third_unlock3=as.Date('2021-05-17'),
                third_unlock4=as.Date('2021-06-21'),
                third_unlock5=as.Date('2022-03-01'))


prais2$lock[prais2$visit_dt<prais2$first_lock] <- 1
prais2$lock[prais2$first_lock<=prais2$visit_dt & prais2$visit_dt<=prais2$first_lock_end] <- 2
prais2$lock[prais2$first_lock_end<prais2$visit_dt & prais2$visit_dt<=prais2$second_lock] <- 3
prais2$lock[prais2$second_lock<=prais2$visit_dt & prais2$visit_dt<=prais2$second_lock_end] <- 4
prais2$lock[prais2$second_lock_end<prais2$visit_dt & prais2$visit_dt<=prais2$third_lock] <- 5
prais2$lock[prais2$third_lock<=prais2$visit_dt & prais2$visit_dt<=prais2$third_unlock4] <- 6
prais2$lock[prais2$third_unlock4<prais2$visit_dt& prais2$visit_dt<=prais2$third_unlock5] <- 7
prais2$lock[prais2$third_unlock5<prais2$visit_dt] <- 8


prais2 <- prais2 %>% 
  dplyr::select(-c("first_lock","first_lock_end","second_lock","second_lock_end","third_lock","third_unlock4","third_unlock5",
                   "week","mon","year","first_lk_wk","first_unlk_wk", "second_lk_wk","second_unlk_wk","third_lk_wk","third_unlock1",
                   "third_unlock2","third_unlock3"))



write.csv(prais2, "ccu007_01_prais2score.csv" ) 

rm("data3")
rm("prais2")
rm("data1")
table(data2$sp_group)
# Creating the HLHS hybird approach procedure group (update sp-group)

data2 <- data2 %>% 
  dplyr::mutate(sp_group=ifelse(diag_rank6==1 &  actv_cat==1 & report_num==4, 1, sp_group))
data2$sp_group[is.na(data2$sp_group)] <- 20


data2 <- data2 %>%  
  dplyr::mutate(com=`2_07_COMORBID_CONDITIONS`) %>% 
  tidyr::separate(`2_07_COMORBID_CONDITIONS`, c("comorb1", "comorb2", "comorb3", "comorb4", "comorb5", "comorb6","comorb7","comorb8" ), ";", extra="merge", fill = "right")


data2 <- data2 %>% 
  tidyr::separate(comorb1, c("morb1", "desc1"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb2, c("morb2", "desc2"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb3, c("morb3", "desc3"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb4, c("morb4", "desc4"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb5, c("morb5", "desc5"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb6, c("morb6", "desc6"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb7, c("morb7", "desc7"), extra = "merge", fill ="right") %>%
  tidyr::separate(comorb8, c("morb8", "desc8"), extra = "merge", fill ="right") 


data2$morb1[(nchar(data2$morb1))<6] <- NA
data2$morb2[(nchar(data2$morb2))<6] <- NA
data2$morb3[(nchar(data2$morb3))<6] <- NA
data2$morb4[(nchar(data2$morb4))<6] <- NA
data2$morb5[(nchar(data2$morb5))<6] <- NA
data2$morb6[(nchar(data2$morb6))<6] <- NA
data2$morb7[(nchar(data2$morb7))<6] <- NA
data2$morb8[(nchar(data2$morb8))<6] <- NA

data2 <- data2 %>% 
  dplyr::select(-c("desc"))
cmorb1 <- merge(data2, cmorb_code, by.x = 'morb1', by.y = 'code', all.x = TRUE)


cmorb1 <- cmorb1 %>% 
  dplyr::mutate(risk_gp1=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

cmorb2 <- merge(cmorb1, cmorb_code, by.x = 'morb2', by.y = 'code', all.x = TRUE)
cmorb2 <- cmorb2 %>% 
  dplyr::mutate(risk_gp2=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))
 
rm("cmorb1")
cmorb3 <- merge(cmorb2, cmorb_code, by.x = 'morb3', by.y = 'code', all.x = TRUE)
cmorb3 <- cmorb3 %>% 
  dplyr::mutate(risk_gp3=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("cmorb2")
cmorb4 <- merge(cmorb3, cmorb_code, by.x = 'morb4', by.y = 'code', all.x = TRUE)
cmorb4 <- cmorb4 %>% 
  dplyr::mutate(risk_gp4=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("cmorb3")
cmorb5 <- merge(cmorb4, cmorb_code, by.x = 'morb5', by.y = 'code', all.x = TRUE)
cmorb5 <- cmorb5 %>% 
  dplyr::mutate(risk_gp5=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

 
rm("cmorb4")
cmorb6 <- merge(cmorb5, cmorb_code, by.x = 'morb6', by.y = 'code', all.x = TRUE)
cmorb6 <- cmorb6 %>% 
  dplyr::mutate(risk_gp6=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("cmorb5")
cmorb7 <- merge(cmorb6, cmorb_code, by.x = 'morb7', by.y = 'code', all.x = TRUE)
cmorb7 <- cmorb7 %>% 
  dplyr::mutate(risk_gp7=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("cmorb6")
cmorb8 <- merge(cmorb7, cmorb_code, by.x = 'morb8', by.y = 'code', all.x = TRUE)
cmorb8 <- cmorb8 %>% 
  dplyr::mutate(risk_gp8=risk_group) %>% 
  dplyr::select(-c("desc", "risk_group"))

rm("cmorb7")




data_prais_rf <- cmorb8 %>% 
  dplyr::mutate(acq_com=ifelse(risk_gp1=="Acquired Comorbidity"|risk_gp2=="Acquired Comorbidity"|risk_gp3=="Acquired Comorbidity"|risk_gp4=="Acquired Comorbidity"|risk_gp5=="Acquired Comorbidity"|risk_gp6=="Acquired Comorbidity"|risk_gp7=="Acquired Comorbidity"|risk_gp8=="Acquired Comorbidity", 1,0),
                acrf_com=ifelse(risk_gp1=="Additional Cardiac Risk Factors"|risk_gp2=="Additional Cardiac Risk Factors"|risk_gp3=="Additional Cardiac Risk Factors"|risk_gp4=="Additional Cardiac Risk Factors"|risk_gp5=="Additional Cardiac Risk Factors"|risk_gp6=="Additional Cardiac Risk Factors"|risk_gp7=="Additional Cardiac Risk Factors"|risk_gp8=="Additional Cardiac Risk Factors", 1,0),
                cong_com=ifelse(risk_gp1=="Congenital Comorbidity"|risk_gp2=="Congenital Comorbidity"|risk_gp3=="Congenital Comorbidity"|risk_gp4=="Congenital Comorbidity"|risk_gp5=="Congenital Comorbidity"|risk_gp6=="Congenital Comorbidity"|risk_gp7=="Congenital Comorbidity"|risk_gp8=="Congenital Comorbidity", 1,0),
                sev_ill_com=ifelse(risk_gp1=="Severity of illness"|risk_gp2=="Severity of illness"|risk_gp3=="Severity of illness"|risk_gp4=="Severity of illness"|risk_gp5=="Severity of illness"|risk_gp6=="Severity of illness"|risk_gp7=="Severity of illness"|risk_gp8=="Severity of illness",1,0))

rm("cmorb8")
data_prais_rf$acq_com[is.na(data_prais_rf$acq_com)] <- 0
data_prais_rf$acrf_com[is.na(data_prais_rf$acrf_com)] <- 0
data_prais_rf$cong_com[is.na(data_prais_rf$cong_com)] <- 0
data_prais_rf$sev_ill_com[is.na(data_prais_rf$sev_ill_com)] <- 0


data_prais_rf <- data_prais_rf %>% 
  dplyr::select( "NHS_NUMBER_DEID","3_01_DATE_OF_VISIT","visit_dt","DATE_OF_BIRTH",
                 "dob","age_yr","2_03_WEIGHT","2_03B_HEIGHT","type_procedure","sp_group",
                 "uniq4","acq_com","acrf_com","cong_com","sev_ill_com" , "prais_rank6")




data_prais_rf <- data_prais_rf %>% 
  dplyr::mutate(diag1=ifelse(prais_rank6==1,1,0),
                diag2=ifelse(prais_rank6==2,1,0),
                diag3=ifelse(prais_rank6==3,1,0),
                diag4=ifelse(prais_rank6==4,1,0),
                diag5=ifelse(prais_rank6==5,1,0),
                diag6=ifelse(prais_rank6==6,1,0),
                diag7=ifelse(prais_rank6==7,1,0),
                diag8=ifelse(prais_rank6==8,1,0),
                diag9=ifelse(prais_rank6==9,1,0),
                diag10=ifelse(prais_rank6==10,1,0),
                diag11=ifelse(prais_rank6==11,1,0),
                bypass=ifelse(type_procedure==1,1,0)
  )
write.csv(data_prais_rf, "ccu007_01_prais2_rf.csv" )

#############################End of prais2################

# activity group <- "nhcda_processed_v6.14.csv"
# specific procedure group <- "sp_algorithm.csv"
# cohort_data <- "ccu007_01_data_extract.csv"
# death_final <- "ccu007_01_death_final.csv"
# diagnosis_final <- "ccu007_01_diagnosis.csv"
# prais2 <- "ccu007_01_prais2score.csv"
# prais2_rf <- "ccu007_01_prais2_rf.csv"

#######################################################################

######################Single dataset #########################

setwd("D:/PhotonUser/My Files/Home Folder/collab/CCU007_01/data")

rm("cmorb_code")
rm("data_prais_rf")
rm("data2")
rm("spc_code1")
library(readr)

actv <- read.csv("nhcda_processed_v6.14.csv")
sp_proc <- read_csv("sp_algorithm.csv") 
cohort <- read_csv("ccu007_01_data_extract.csv")
death_final <- read_csv("ccu007_01_death_final.csv")
diagn <- read_csv("ccu007_01_diagnosis.csv")
prais <- read_csv("ccu007_01_prais2score.csv")
prais_rf <- read_csv("ccu007_01_prais2_rf.csv")
mmc1 <- read_csv("mmc1.csv")


actv <- actv %>% 
  dplyr::mutate(report_num=ifelse(report_group=="bypass",1,
                                  ifelse(report_group=="diagnostic:non-surgical",2,
                                         ifelse(report_group=="ep:non-surgical",3,
                                                ifelse(report_group=="hybrid",4,
                                                       ifelse(report_group=="icd:non-surgical",5,
                                                              ifelse(report_group=="intervention:non-surgical",6,
                                                                     ifelse(report_group=="no_qualifying_codes", 7, 
                                                                            ifelse(report_group=="non-bypass",8,
                                                                                   ifelse(report_group=="pacemaker:non-surgical", 9,
                                                                                          ifelse(report_group=="primary_ecmo",10,
                                                                                                 ifelse(report_group=="unallocated",11,
                                                                                                        ifelse(report_group=="unallocated-ecmo",12,
                                                                                                               13)))))))))))))

actv <- actv %>% 
  dplyr::mutate(actv_cat=ifelse(report_num==1|report_num==4|report_num==8,1,
                                ifelse(report_num==6,2,
                                       ifelse(report_num==2,3,
                                              ifelse(report_num==3|report_num==5|report_num==9,4,
                                                     ifelse(report_num==7, 5,6))))),
                actv_cat_name=ifelse(actv_cat==1, "Surgery",
                                     ifelse(actv_cat==2, "Intv_cath",
                                            ifelse(actv_cat==3, "Diag_cath",
                                                   ifelse(actv_cat==4, "Ep",
                                                          ifelse(actv_cat==5, "Chest_cl_expl",
                                                                 "Mechanical_support"))))))

 d1 <- actv %>% 
  dplyr::select("patient_identifier", "uniq4", "type_procedure", "age_group", "report_group", "report_num", "actv_cat", "actv_cat_name")

 rm("actv")



d2 <- sp_proc %>% 
  dplyr::rename("type_procedure"="type_procedure.y") %>% 
  dplyr::select("uniq4", "type_procedure", "sp_algorithm_group")

d3 <-  death_final %>% 
  dplyr::select("uniq4","cod_cat", "cod1_cat", "cod2_cat", "death", "related", "related_cat", "death_cat")
rm("death_final")

diagn <- diagn %>% 
  dplyr::mutate(uvh_final=ifelse(uvh1=="Yes"|uvh2=="Yes"|uvh3=="Yes"|uvh4=="Yes"|uvh5=="Yes"|uvh6=="Yes"|uvh7=="Yes"|uvh8=="Yes"|uvh9=="Yes"|uvh10=="Yes",1,0))

diagn$uvh_final[is.na(diagn$uvh_final)] <- 0
table(diagn$lock)

d4 <- diagn %>% 
  dplyr::select( "visit_dt", "uniq4", "diag_rank", "diag_rank6", "prais_rk", "uvh_final")
rm("diagn")


d5 <- prais %>% 
  dplyr::select( "prais2", "uniq4")
rm("prais")
d6 <- prais_rf %>% 
  dplyr::select("uniq4","acq_com","acrf_com","cong_com","sev_ill_com" , "prais_rank6")
rm("prais_rf")

d12 <- merge(d1, d2, by.x = "uniq4", by.y = "uniq4", all = TRUE)
d123 <- merge(d12, d3, by.x = "uniq4", by.y = "uniq4", all = TRUE)
d1234 <- merge(d123, d4, by.x = "uniq4", by.y = "uniq4", all = TRUE)
d12345 <- merge(d1234, d5, by.x = "uniq4", by.y = "uniq4", all = TRUE)
d123456 <- merge(d12345, d6, by.x = "uniq4", by.y = "uniq4", all = TRUE)

rm("d1")
rm("d2")
rm("d3")
rm("d12")
rm("d4")
rm("d123")
rm("d5")
rm("d1234")
rm("d6")
rm("d12345")



mmc_dg <- mmc1 %>% 
  group_by(`diagnosis group`) %>% 
  dplyr::summarise(rank_dg=mean(`diagnosis group rank`))

d123456 <- merge(d123456, mmc_dg, by.x = "diag_rank6", by.y="rank_dg", all.x = TRUE)
rm("d12")
rm("d123")
rm("d1234")
rm("d12345")
# bringing in the cohort varaibles-----------------------------------------------------------
cohort  <- data1  %>%
  tidyr::separate(`4_08_POST_OPERATIVE_COMPLICATIONS`, c("com_1", "com_2", "com_3", "com_4"),";")

cohort <- cohort %>% 
  separate(com_1, c("com1", "com_1_des")) %>% 
  separate(com_2, c("com2", "com_2_des")) %>% 
  separate(com_3, c("com3", "com_3_des")) %>% 
  separate(com_4, c("com4", "com_4_des")) 

cohort <- cohort %>%
  separate(`7_05_CATHETERISATION_COMPLICATIONS`, c("cat_com_1", "cat_com_2", "cat_com_3", "cat_com_4"), ";")



cohort <- cohort %>% 
  separate(cat_com_1, c("cat_com1", "cat_com_desc1")) %>% 
  separate(cat_com_2, c("cat_com2", "cat_com_desc2")) %>% 
  separate(cat_com_3, c("cat_com3", "cat_com_desc3")) 


#Complication indicators

cohort <- cohort %>% 
  dplyr::mutate(com_unplan=ifelse(com1=="124307"|com2=="124307"|com3=="124307"|com4=="124307",1,0),
                com_ecmo=ifelse(com1=="150009"|com2=="150009"|com3=="150009"|com4=="150009",1,0),
                com_necro=ifelse(com1=="158375"|com2=="158375"|com3=="158375"|com4=="158375",1,0),
                com_site=ifelse(com1=="156741"|com2=="156741"|com3=="156741"|com4=="156741",1,0),
                com_pleural=ifelse(com1=="158064"|com2=="158064"|com3=="158064"|com4=="158064"|
                                     com1=="158065"|com2=="158065"|com3=="158065"|com4=="158065",1,0),
                com_other=ifelse(com1=="110633"|com2=="110633"|com3=="110633"|com4=="110633"|
                                   com1=="158086"|com2=="158086"|com3=="158086"|com4=="158086"|
                                   com1=="158190"|com2=="158190"|com3=="158190"|com4=="158190"|
                                   com1=="158213"|com2=="158213"|com3=="158213"|com4=="158213"|
                                   com1=="158257"|com2=="158257"|com3=="158257"|com4=="158257"|
                                   com1=="159014"|com2=="159014"|com3=="159014"|com4=="159014",1,0))



cohort$com_unplan[is.na(cohort$com_unplan)] <- 0
cohort$com_ecmo[is.na(cohort$com_ecmo)] <- 0
cohort$com_necro[is.na(cohort$com_necro)] <- 0
cohort$com_site[is.na(cohort$com_site)] <- 0
cohort$com_pleural[is.na(cohort$com_pleural)] <- 0
cohort$com_other[is.na(cohort$com_other)] <- 0


cohort <- cohort %>% 
  dplyr::mutate(comp_total=com_unplan+com_ecmo+com_necro+com_site+com_pleural+com_other,
                complcn=ifelse(comp_total==0,0,1))



com_other<- table(cohort$`4_03_DISCHARGE_STATUS`)
cbind(com_other,prop.table(com_other))
rm(com_other)

# cov_cat_deprivation
cohort$cov_deprivation <- NA
cohort$cov_deprivation[(cohort$DECI_IMD)==1|(cohort$DECI_IMD)==2 ] <-"1-2 (most deprived)"
cohort$cov_deprivation[(cohort$DECI_IMD)==3|(cohort$DECI_IMD)==4 ] <-"3-4"
cohort$cov_deprivation[(cohort$DECI_IMD)==5|(cohort$DECI_IMD)==6 ] <-"5-6"
cohort$cov_deprivation[(cohort$DECI_IMD)==7|(cohort$DECI_IMD)==8 ] <-"7-8"
cohort$cov_deprivation[(cohort$DECI_IMD)==9|(cohort$DECI_IMD)==10 ] <-"9-10 (least deprived)"
cohort$cov_deprivation <- ordered(cohort$cov_deprivation, levels = c("1-2 (most deprived)","3-4","5-6","7-8","9-10 (least deprived)"))



cohort <- cohort %>% 
  dplyr::mutate(procedure_date=as.Date(`3_01_DATE_OF_VISIT`),
                proc_hr=hour(`3_01_DATE_OF_VISIT`),
                proc_min=minute(`3_01_DATE_OF_VISIT`),
                proc_time=paste(proc_hr, proc_min, sep=":")) %>% 
  dplyr::mutate(uniq1=paste(`1_03_NHS_NUMBER_DEID`, visit_dt),
                uniq2=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time),
                uniq3=paste(`1_03_NHS_NUMBER_DEID`,visit_dt, proc_time),
                uniq4=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`),
                uniq5=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`, `4_01_DATE_OF_DISCHARGE` ))



cohort <- cohort %>% 
  dplyr::select("gender","eth_cat","region_name","3_01B_PROCEDURE_URGENCY","complcn",
                "com_unplan","com_ecmo","com_necro","com_site","com_pleural","com_other" ,
                "3_01C_UNPLANNED_REOPERATION","4_05_POST_PROCEDURE_SEIZURES",
                "7_04_CATHETERISATION_COMPLICATION_SEVERITY_RATING","4_03_DISCHARGE_STATUS","4_01_DATE_OF_DISCHARGE",
                "4_04_DISCHARGE_DESTINATION", "uniq4", "1_03_NHS_NUMBER_DEID", "cov_deprivation", "DECI_IMD")



data_final <- merge(d123456, cohort, by.x = "uniq4", by.y = "uniq4")

rm("d123456")
rm("cohort")
data_final <- data_final %>% 
  dplyr::mutate(cardiac=ifelse(report_num==1|report_num==8| report_num==4, 1, 0),
                intervn_cath=ifelse(report_num==6,1,0),
                ep=ifelse(report_num==3|report_num==5| report_num==9,1,0),
                diag_cath=ifelse(report_num==2,1,0))

data_final <- data_final %>% 
  dplyr::mutate(week=week(visit_dt),
                mon= month(visit_dt),
                year=year(visit_dt),
                first_lock=as.Date('2020-03-23'),
                first_lk_wk=week(first_lock),
                first_lock_end=as.Date('2020-06-23'),
                first_unlk_wk=week(first_lock_end),
                second_lock=as.Date('2020-11-05'),
                second_lk_wk=week(second_lock),
                second_lock_end=as.Date('2020-12-02'),
                second_unlk_wk=week(second_lock_end),
                third_lock=as.Date('2021-01-06'),
                third_lk_wk=week(third_lock),
                third_unlock1=as.Date('2021-03-08'),
                third_unlock2=as.Date('2021-03-12'),
                third_unlock3=as.Date('2021-05-17'),
                third_unlock4=as.Date('2021-06-21'),
                third_unlock5=as.Date('2022-03-01'))


data_final$lock[data_final$visit_dt<data_final$first_lock] <- 1
data_final$lock[data_final$first_lock<=data_final$visit_dt & data_final$visit_dt<=data_final$first_lock_end] <- 2
data_final$lock[data_final$first_lock_end<data_final$visit_dt & data_final$visit_dt<=data_final$second_lock] <- 3
data_final$lock[data_final$second_lock<=data_final$visit_dt & data_final$visit_dt<=data_final$second_lock_end] <- 4
data_final$lock[data_final$second_lock_end<data_final$visit_dt & data_final$visit_dt<=data_final$third_lock] <- 5
data_final$lock[data_final$third_lock<=data_final$visit_dt & data_final$visit_dt<=data_final$third_unlock4] <- 6
data_final$lock[data_final$third_unlock4<data_final$visit_dt] <- 7

data_final <- data_final %>% 
  dplyr::select(-c("first_lock", "week", "mon", "first_lk_wk","first_lock_end", "first_unlk_wk", "second_lock",
                   "second_lk_wk","year",
                   "second_lock_end",
                   "second_unlk_wk",
                   "third_lock",
                   "third_unlock1",
                   "third_unlock2",
                   "third_unlock3",
                   "third_unlock4",
                   "third_unlock5"))

d7 <- read_csv("ccu007_01_data_extract.csv")

d7 <- d7 %>% 
  dplyr::select(`3_01_DATE_OF_VISIT`,`1_03_NHS_NUMBER_DEID`,`8_01_PROCEDURE_UNIQUE_ID`,`DATE_OF_BIRTH`)
d7 <- d7 %>% 
  dplyr::mutate(visit_dt=as.Date(`3_01_DATE_OF_VISIT`),
                proc_hr=hour(`3_01_DATE_OF_VISIT`),
                proc_min=minute(`3_01_DATE_OF_VISIT`),
                proc_time=paste(proc_hr, proc_min, sep=":")) %>% 
  dplyr::mutate(
    uniq4=paste(`1_03_NHS_NUMBER_DEID`, visit_dt, proc_time,`8_01_PROCEDURE_UNIQUE_ID`))

d7 <- d7 %>% 
  dplyr::mutate(dob=as.Date(`DATE_OF_BIRTH`),
                age_years=(visit_dt-dob)/365.25)
d7$age_years <- as.numeric(d7$age_years)

d7$age_yr <- abs(round(d7$age_years, digits = 3))

d7 <- d7 %>%
  dplyr::mutate(aged=age_yr*365.25,
                agegp=ifelse(aged<365.25, 1,
                             ifelse(aged>=365.25 & aged<1826.25,2,
                                    ifelse(aged>=1826.25 & aged<3652.5,3,
                                           ifelse(aged>=3652.5 & !is.na(aged),4, NA)))))






d7 <- d7 %>% 
  dplyr::mutate(child = ifelse(age_yr<16.0,1,0)) %>% 
  dplyr::filter(child==1)


d7 <- d7 %>% 
  dplyr::mutate(year=year(visit_dt),mon=month(visit_dt),unselect=ifelse(year==2022 & mon>3,1,0)) %>% 
  dplyr::filter(unselect!=1) %>% 
  dplyr::select(-c(unselect)) 
d7 <- d7 %>% 
  dplyr::select(uniq4, age_yr, agegp, dob)
data_final <- merge(data_final,d7, by="uniq4")
data_final <- data %>% 
  dplyr::mutate(lock=ifelse(lock==8,7,lock))
rm("d7")
write.csv(data_final, "ccu007_01_analysisdataset3.csv" )




##################tables and figures##########################################
setwd("/mnt/efs/arun.k.suseeladevi/CCU007_01/data")
library(readr)
library(dplyr)
data_final <- read_csv("ccu007_01_analysisdataset.csv")

data_final_uniq <- data_final %>% 
  dplyr::group_by(`1_03_NHS_NUMBER_DEID`) %>% 
  dplyr::mutate(nrow=row_number()) %>% 
  ungroup() %>% 
  dplyr::filter(nrow==1)

#procedure tables

gender<- table(data_final$`gender`)
cbind(gender,prop.table(gender))
rm(gender)

deprivation <- table(data_final$cov_deprivation)
cbind(deprivation,prop.table(deprivation))
rm(deprivation)


eth_cat<- table(data_final$`eth_cat`)
cbind(eth_cat,prop.table(eth_cat))
rm(eth_cat)

region_name<- table(data_final$`region_name`)
cbind(region_name,prop.table(region_name))
rm(region_name)

diagnosis<- table(data_final$`diagnosis group`)
cbind(diagnosis,prop.table(diagnosis))
rm(diagnosis)

x <- data_final %>% 
  dplyr::group_by(`diagnosis group`) %>% 
  dplyr::summarise(count=n())
table(data_final$`3_01B_PROCEDURE_URGENCY`)

urgency<- table(data_final$`3_01B_PROCEDURE_URGENCY`)
cbind(urgency,prop.table(urgency))
rm(urgency)

table(data_final$actv_cat_name)

activity<- table(data_final$actv_cat_name)
cbind(activity,prop.table(activity))
rm(activity)

compl<- table(data_final$complcn)
cbind(compl,prop.table(compl))
rm(compl)

comp_ecmo<- table(data_final$com_ecmo)
cbind(comp_ecmo,prop.table(comp_ecmo))
rm(comp_ecmo)

comp_unplan<- table(data_final$com_unplan)
cbind(comp_unplan,prop.table(comp_unplan))
rm(comp_unplan)

comp_necro<- table(data_final$com_necro)
cbind(comp_necro,prop.table(comp_necro))
rm(comp_necro)


comp_surg<- table(data_final$com_site)
cbind(comp_surg,prop.table(comp_surg))
rm(comp_surg)

comp_pleur<- table(data_final$com_pleural)
cbind(comp_pleur,prop.table(comp_pleur))
rm(comp_pleur)

comp_oth<- table(data_final$com_other)
cbind(comp_oth,prop.table(comp_oth))
rm(comp_oth)


unplan<- table(data_final$`3_01C_UNPLANNED_REOPERATION`)
cbind(unplan,prop.table(unplan))
rm(unplan)




table(data_final$`4_03_DISCHARGE_STATUS`)
disch_dest<- table(data_final$`4_04_DISCHARGE_DESTINATION`)
cbind(disch_dest,prop.table(disch_dest))
rm(disch_dest)

disch_stat<- table(data_final$`4_03_DISCHARGE_STATUS`)
cbind(disch_stat,prop.table(disch_stat))
rm(disch_stat)


data_final$durn <- as.numeric((data_final$`4_01_DATE_OF_DISCHARGE`-data_final$visit_dt))

summary(data_final$durn)


quantile(data_final$durn, probs = c(0.25,0.5, 0.75), na.rm=TRUE)
ls(data3)

#individual numbers

gender<- table(data_final_uniq$`gender`)
cbind(gender,prop.table(gender))
rm(gender)

eth_cat<- table(data_final_uniq$`eth_cat`)
cbind(eth_cat,prop.table(eth_cat))
rm(eth_cat)

region_name<- table(data_final_uniq$`region_name`)
cbind(region_name,prop.table(region_name))
rm(region_name)

deprivation <- table(data_final_uniq$cov_deprivation)
cbind(deprivation,prop.table(deprivation))
rm(deprivation)


#calculation of the rates
rm(list=setdiff(ls(), "data_final"))



table(data_final$`4_04_DISCHARGE_DESTINATION`, data_final$lock)

table(data_final$`4_03_DISCHARGE_STATUS`, data_final$lock)

data_final <- data_final %>%
  
  dplyr::mutate(d1=ifelse(`4_03_DISCHARGE_STATUS`=="D. Died in hospital",1,0),
                d2=ifelse(`4_04_DISCHARGE_DESTINATION`=="4. Death"|`4_04_DISCHARGE_DESTINATION`=="5. Death with referral to coroner",1,0),
                d3=ifelse(death==1 & related_cat==1,1,0),
                d4=ifelse(death_cat==1 & related_cat==1,1,0))





# diagnosis_group ---------------------------------------------------------


table(data$`diagnosis group`)

data_final <- data_final %>% 
  dplyr::mutate(diagn_gp=ifelse(`diagnosis group`=="Pulmonary stenosis"|`diagnosis group`=="Pulmonary atresia and IVS"|`diagnosis group`=="TAPVC", "Pulmonary related CHD",
                                ifelse(`diagnosis group`=="Aortic valve stenosis (isolated)"|`diagnosis group`=="Tricuspid valve abnormality (including Ebstein's)"|`diagnosis group`=="Mitral valve abnormality (including supravalvar, subvalvar)", "Valvular CHD",
                                       ifelse(`diagnosis group`=="Interrupted aortic arch"|`diagnosis group`=="Subaortic stenosis (isolated)"|`diagnosis group`=="Aortic regurgitation", "Aortic CHD",
                                              ifelse(`diagnosis group`=="Transposition of great arteries (concordant AV & discordant VA connections) & IVS"|`diagnosis group`=="TGA+VSD/ DORV-TGA type"|`diagnosis group`=="Common arterial trunk (truncus arteriosus)","Great artery related CHD", `diagnosis group`)))))



table(data_final$diagn_gp)



# Updating the table (complication, death model - prais and RF) -----------

###MAin model
# 4. Outcome 4. Proportion of complications


data <- data_final %>% 
  dplyr::filter(actv_cat!=5)

rm("data_final")


## fitting logistic regression



# age adjusted model

com_model_age <- glm(complcn~as.factor(lock)+as.factor(agegp), family="binomial", data=data)
com_model_age_rob <- coeftest(com_model_age,
                              vcov=vcovHC)

com_model_age_rob <- tidy(com_model_age_rob)
com_model_age_rob<- com_model_age_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
com_model_age_rob$model <- "age"

com_model <- com_model_age_rob
com_model$out <- "complcn"
com_model$strata <- "main"
# 5. Outcome 5. Proportion of death

## fitting logistic regression Death


# age-adjusted model
data2 <- data %>% 
  dplyr::filter(!is.na(prais2))

d_model_age <- glm(d1~as.factor(lock)+as.factor(agegp), family="binomial", data=data)
d_model_age_rob <- coeftest(d_model_age,
                            vcov=vcovHC)

d_model_age_rob <- tidy(d_model_age_rob)
d_model_age_rob<- d_model_age_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_rob$model <- "age"

# age-prias2 adjusted model

d_model_age_pr <- glm(d1~as.factor(lock)+as.factor(agegp)+prais2, family="binomial", data=data2)
d_model_age_pr_rob <- coeftest(d_model_age_pr,
                               vcov=vcovHC)

d_model_age_pr_rob <- tidy(d_model_age_pr_rob)
d_model_age_pr_rob<- d_model_age_pr_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_pr_rob$model <- "age + prais2"

# age-prias_rf adjusted model

d_model_age_rf <- glm(d1~as.factor(lock)+as.factor(agegp)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+as.factor(acrf_com) + as.factor(prais_rk), family="binomial", data=data)
d_model_age_rf_rob <- coeftest(d_model_age_rf,
                               vcov=vcovHC)

d_model_age_rf_rob <- tidy(d_model_age_rf_rob)
d_model_age_rf_rob<- d_model_age_rf_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_rf_rob$model <- "age + prais_rf"

death_model <- rbind(d_model_age_rob,d_model_age_pr_rob, d_model_age_rf_rob)
death_model$out <- "death"
death_model$strata <- "main"

models <- rbind( com_model, death_model)
setwd("~/CCU007_01/results")

write_csv(models, paste0("ccu007_01_main_OR.csv"))



##stratified analysis for the Urgency, complcn, and death models

#1. Age group 1-------

rm(list = setdiff(ls(), c("data", "models")) )


data1 <- data %>% 
  dplyr::filter(agegp==1)
# 4. Outcome 4. Proportion of complications


## fitting logistic regression





# age- sex adjusted model

com_model_age <- glm(complcn~as.factor(lock), family="binomial", data=data1)
com_model_age_rob <- coeftest(com_model_age,
                              vcov=vcovHC)

com_model_age_rob <- tidy(com_model_age_rob)
com_model_age_rob<- com_model_age_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
com_model_age_rob$model <- "age"


com_model <-  com_model_age_rob
com_model$out <- "complcn"
com_model$strata <- "age1"

# 5. Outcome 5. Proportion of death

## fitting logistic regression Death



# age- sex adjusted model

d_model_age <- glm(d1~as.factor(lock), family="binomial", data=data1)
d_model_age_rob <- coeftest(d_model_age,
                            vcov=vcovHC)

d_model_age_rob <- tidy(d_model_age_rob)
d_model_age_rob<- d_model_age_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_rob$model <- "age"



# age-prias2 adjusted model
data2 <- data1 %>% 
  dplyr::filter(!is.na(prais2))
d_model_age_pr <- glm(d1~as.factor(lock)+prais2, family="binomial", data=data2)
d_model_age_pr_rob <- coeftest(d_model_age_pr,
                               vcov=vcovHC)

d_model_age_pr_rob <- tidy(d_model_age_pr_rob)
d_model_age_pr_rob<- d_model_age_pr_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_pr_rob$model <- "age + prais2"

# age-prias_rf adjusted model


d_model_age_rf <- glm(d1~as.factor(lock)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+as.factor(acrf_com)+ as.factor(prais_rk), family="binomial", data=data1)
d_model_age_rf_rob <- coeftest(d_model_age_rf,
                               vcov=vcovHC)

d_model_age_rf_rob <- tidy(d_model_age_rf_rob)
d_model_age_rf_rob<- d_model_age_rf_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_rf_rob$model <- "age + prais_rf"

death_model <- rbind(d_model_age_rob,d_model_age_pr_rob, d_model_age_rf_rob)
death_model$out <- "death"
death_model$strata <- "age1"
models <- rbind(models,com_model, death_model)

# Similarly repeated for subgroups for age, deprivation, adn ethnicity -------

setwd("~/CCU007_01/results")

write_csv(models, paste0("ccu007_01_all_OR.csv"))




# Age group -interaction p value ------------------------------
rm(list = setdiff(ls(), c("con", "data")))



# 4. Outcome  complications
com_model_age <- glm(complcn~as.factor(lock)+as.factor(agegp), family="binomial", data=data)
com_model_age_int <- glm(complcn~as.factor(lock)+as.factor(agegp) + as.factor(lock):as.factor(agegp), family="binomial", data=data)

com_age_lr <- lrtest(com_model_age, com_model_age_int)

# 5. Outcome  death

d_model_age <- glm(d1~as.factor(lock)+as.factor(agegp)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+as.factor(acrf_com)+ as.factor(prais_rk), family="binomial", data=data)
d_model_age_int <- glm(d1~as.factor(lock)+as.factor(agegp) +as.factor(lock):as.factor(agegp)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+as.factor(acrf_com)+ as.factor(prais_rk), family="binomial", data=data)

d_age_lr <- lrtest(d_model_age, d_model_age_int)

#lrt results

com_age_lr$out <- "com"
com_age_lr <- com_age_lr %>% 
  dplyr::filter(!is.na(Chisq)) %>% 
  dplyr::select("out", "Pr(>Chisq)")

d_age_lr$out <- "d"
d_age_lr <- d_age_lr %>% 
  dplyr::filter(!is.na(Chisq)) %>% 
  dplyr::select("out", "Pr(>Chisq)")

age_lr <- rbind(com_age_lr, d_age_lr)
age_lr$strat <- "age"


# Deprivation group -interaction p value

table(data$cov_deprivation)

# 4. Outcome  complications
com_model_depr <- glm(complcn~as.factor(lock)+as.factor(agegp)+as.factor(cov_deprivation), family="binomial", data=data)
com_model_depr_int <- glm(complcn~as.factor(lock)+as.factor(agegp) +as.factor(cov_deprivation)+ as.factor(lock):as.factor(cov_deprivation), family="binomial", data=data)

com_depr_lr <- lrtest(com_model_depr, com_model_depr_int)

# 5. Outcome  death

d_model_depr <- glm(d1~as.factor(lock)+as.factor(agegp)+as.factor(cov_deprivation)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+ as.factor(prais_rk)+as.factor(acrf_com), family="binomial", data=data)
d_model_depr_int <- glm(d1~as.factor(lock)+as.factor(agegp) +as.factor(cov_deprivation)+as.factor(lock):as.factor(cov_deprivation)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+ as.factor(prais_rk)+as.factor(acrf_com), family="binomial", data=data)

d_depr_lr <- lrtest(d_model_depr, d_model_depr_int)

#lrt results


com_depr_lr$out <- "com"
com_depr_lr <- com_depr_lr %>% 
  dplyr::filter(!is.na(Chisq)) %>% 
  dplyr::select("out", "Pr(>Chisq)")

d_depr_lr$out <- "d"
d_depr_lr <- d_depr_lr %>% 
  dplyr::filter(!is.na(Chisq)) %>% 
  dplyr::select("out", "Pr(>Chisq)")

depr_lr <- rbind( com_depr_lr, d_depr_lr)
depr_lr$strat <- "depr"


# Ethnic group



data <- data %>% 
  dplyr::mutate(ethn_gp=ifelse(eth_cat=="African/Caribbean", "African",
                               ifelse(eth_cat=="South Asian", "South Asian",
                                      ifelse(eth_cat=="White", "White",
                                             ifelse(!is.na(eth_cat), "Other",NA)))))
table(data$ethn_gp)

# 4. Outcome  complications
com_model_eth <- glm(complcn~as.factor(lock)+as.factor(agegp)+as.factor(ethn_gp), family="binomial", data=data)
com_model_eth_int <- glm(complcn~as.factor(lock)+as.factor(agegp) +as.factor(ethn_gp)+ as.factor(lock):as.factor(ethn_gp), family="binomial", data=data)

com_eth_lr <- lrtest(com_model_eth, com_model_eth_int)

# 5. Outcome  death

d_model_eth <- glm(d1~as.factor(lock)+as.factor(agegp)+as.factor(ethn_gp)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+ as.factor(prais_rk)+as.factor(acrf_com), family="binomial", data=data)
d_model_eth_int <- glm(d1~as.factor(lock)+as.factor(agegp) +as.factor(ethn_gp)+as.factor(lock):as.factor(ethn_gp)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+ as.factor(prais_rk)+as.factor(acrf_com), family="binomial", data=data)

d_eth_lr <- lrtest(d_model_eth, d_model_eth_int)

#lrt results



com_eth_lr$out <- "com"
com_eth_lr <- com_eth_lr %>% 
  dplyr::filter(!is.na(Chisq)) %>% 
  dplyr::select("out", "Pr(>Chisq)")

d_eth_lr$out <- "d"
d_eth_lr <- d_eth_lr %>% 
  dplyr::filter(!is.na(Chisq)) %>% 
  dplyr::select("out", "Pr(>Chisq)")

eth_lr <- rbind( com_eth_lr, d_eth_lr)
eth_lr$strat <- "eth"

lrt_output <- rbind(age_lr, depr_lr, eth_lr)


setwd("~r/CCU007_01/results")


write.csv(lrt_output, "ccu007_01_lrt_pval.csv")





# New supplementary figure for mortality analysis limiting to cardiac surgical procedures

## fitting logistic regression Death
setwd("~/CCU007_01/data")

data <- read_csv("ccu007_01_analysisdataset.csv")

data <- data %>% 
  dplyr::filter(actv_cat!=5)# removing chest closure and exploration

data2 <- data %>% 
  dplyr::filter(!is.na(prais2)) %>% 
  dplyr::mutate(d1=ifelse(`4_03_DISCHARGE_STATUS`=="D. Died in hospital",1,0))

#unadjusted
d_model_unadj <- glm(d1~as.factor(lock), family="binomial", data=data2)
d_model_unadj_rob <- coeftest(d_model_unadj,
                            vcov=vcovHC)

d_model_unadj_rob <- tidy(d_model_unadj_rob)
d_model_unadj_rob<- d_model_unadj_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_unadj_rob$model <- "unadj"

# age-adjusted model
d_model_age <- glm(d1~as.factor(lock)+as.factor(agegp), family="binomial", data=data2)
d_model_age_rob <- coeftest(d_model_age,
                            vcov=vcovHC)

d_model_age_rob <- tidy(d_model_age_rob)
d_model_age_rob<- d_model_age_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_rob$model <- "age"

# age-prias2 adjusted model

d_model_age_pr <- glm(d1~as.factor(lock)+as.factor(agegp)+prais2, family="binomial", data=data2)
d_model_age_pr_rob <- coeftest(d_model_age_pr,
                               vcov=vcovHC)

d_model_age_pr_rob <- tidy(d_model_age_pr_rob)
d_model_age_pr_rob<- d_model_age_pr_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_pr_rob$model <- "age + prais2"

# age-prias_rf adjusted model

d_model_age_rf <- glm(d1~as.factor(lock)+as.factor(agegp)+as.factor(cong_com)+as.factor(sev_ill_com)+as.factor(acq_com)+as.factor(acrf_com) + as.factor(prais_rk), family="binomial", data=data2)
d_model_age_rf_rob <- coeftest(d_model_age_rf,
                               vcov=vcovHC)

d_model_age_rf_rob <- tidy(d_model_age_rf_rob)
d_model_age_rf_rob<- d_model_age_rf_rob %>% 
  dplyr::filter(term=="as.factor(lock)2"|term=="as.factor(lock)3"|term=="as.factor(lock)4"|term=="as.factor(lock)5"|term=="as.factor(lock)6"|term=="as.factor(lock)7") %>% 
  dplyr::mutate(lci=exp(estimate-(qnorm(0.975)*std.error)),
                uci=exp(estimate+(qnorm(0.975)*std.error)),
                or=exp(estimate)) %>% 
  dplyr::select(term, or, lci, uci)
d_model_age_rf_rob$model <- "age + prais_rf"

death_model <- rbind(d_model_unadj_rob,d_model_age_rob,d_model_age_pr_rob, d_model_age_rf_rob)
death_model$out <- "death"
death_model$strata <- "main"

models <- rbind(death_model)
setwd("D:/PhotonUser/My Files/Home Folder/CCU007_01/results")

write_csv(models, paste0("ccu007_01_death_OR_sens.csv"))


