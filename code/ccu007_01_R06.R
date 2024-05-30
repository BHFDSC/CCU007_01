#==============================================================================
######### PER PROTCOL ANALYSIS ## Data for Fig 2,3,4  & Supplementary Figure S1
#==============================================================================
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(ggplot2)
library(nnet)
library(plyr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(sandwich)
library(lmtest)
library(broom)
library(MASS)
setwd("~/CCU007_01/data") 




rm(list=ls())

data <- read_csv("ccu007_01_analysisdataset.csv")

data <- data %>%
  dplyr::filter(actv_cat!=5)

data <- data %>%
  dplyr::mutate(d1=ifelse(`4_03_DISCHARGE_STATUS`=="D. Died in hospital",1,0),
                d2=ifelse(`4_04_DISCHARGE_DESTINATION`=="4. Death"|`4_04_DISCHARGE_DESTINATION`=="5. Death with referral to coroner",1,0),
                d3=ifelse(death==1 & related_cat==1,1,0),
                d4=ifelse(death_cat==1 & related_cat==1,1,0))




# creating week variable and constucting weekly count of procedures so that we can estimate mean numbe rof procedures per week and 95%CI

data <- data %>% 
  dplyr::mutate(year=year(visit_dt),
                mon=month(visit_dt),
                week=week(visit_dt))



wk_count <- data %>% 
  dplyr::group_by(lock, year, mon,week) %>% 
  dplyr::summarise(
    count=n()) %>% 
  ungroup()

wk_count <- data %>% 
  dplyr::group_by(lock, year, week) %>% 
  dplyr::summarise(st_dt=min(visit_dt),
                   ed_dt=max(visit_dt),
                   count=n()) %>% 
  ungroup()
wk_count <- data %>% 
  dplyr::group_by(lock, year, week) %>% 
  dplyr::summarise(st_dt=min(visit_dt),
                   ed_dt=max(visit_dt),
                   count=n()) %>% 
  ungroup() %>% 
  dplyr::mutate(wk_d=as.numeric(ed_dt-st_dt)+1,
                mult=7/wk_d,
                event=count*mult) %>% 
  dplyr::mutate(total=sum(count)) %>% 
  dplyr::group_by(lock) %>% 
  dplyr::mutate(lk_total=sum(count),
                mn_count=mean(count),
                sd_count=sd(count),
                mn_event=mean(event),
                sd_event=sd(event))

wk_count <- data %>% 
  dplyr::group_by(lock, year, week) %>% 
  dplyr::summarise(st_dt=min(visit_dt),
                   ed_dt=max(visit_dt),
                   count=n(),
                   wk_d=n_distinct(visit_dt)) %>% 
  ungroup() 

wk_count <- wk_count %>% 
  dplyr::arrange(lock, year,week) %>% 
  dplyr::mutate(lk_nxt=lead(lock))
wk_count <- wk_count %>% 
  dplyr::mutate(
                mult=7/wk_d,
                event=ifelse(lock!=lk_nxt,count*mult,count)) %>% 
  dplyr::mutate(total=sum(count)) %>% 
  dplyr::group_by(lock) %>% 
  dplyr::mutate(lk_total=sum(count),
                mn_count=mean(count),
                sd_count=sd(count),
                mn_event=mean(event),
                sd_event=sd(event))

library(ggplot2)



##################Wilcoxon signed rank test#####################################

# Overall number of procedures

wk_med <- wk_count %>% 
  dplyr::group_by(lock) %>% 
  get_summary_stats(event, show=c("median","q1", "q3", "iqr"))


a <- wk_count$event[wk_count$lock==1]
b <- wk_count$event[wk_count$lock==2]
c <- wk_count$event[wk_count$lock==3]
d <- wk_count$event[wk_count$lock==4]
e <- wk_count$event[wk_count$lock==5]
f <- wk_count$event[wk_count$lock==6]
g <- wk_count$event[wk_count$lock==7]


shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)
shapiro.test(e)
shapiro.test(f)
shapiro.test(g)

ab <- wilcox.test(a, b,
            paired = FALSE)
ac <- wilcox.test(a, c,
            paired = FALSE)
ad <- wilcox.test(a, d,
            paired = FALSE)
ae <- wilcox.test(a, e,
            paired = FALSE)
af <- wilcox.test(a, f,
            paired = FALSE)
ag <- wilcox.test(a, g,
            paired = FALSE)
rm(a_p)
p.value <- 1
a_p <- as.data.frame(p.value)
ab_p <- as.data.frame(ab$p.value)
ac_p <- as.data.frame(ac$p.value)
ad_p <- as.data.frame(ad$p.value)
ae_p <- as.data.frame(ae$p.value)
af_p <- as.data.frame(af$p.value)
ag_p <- as.data.frame(ag$p.value)


a_p$lock <- 1
ab_p$lock <- 2
ac_p$lock <- 3
ad_p$lock <- 4
ae_p$lock <- 5
af_p$lock <- 6
ag_p$lock <- 7
ab_p <- ab_p %>% 
  dplyr::rename(p.value=`ab$p.value`)
ac_p <- ac_p %>% 
  dplyr::rename(p.value=`ac$p.value`)
ad_p <- ad_p %>% 
  dplyr::rename(p.value=`ad$p.value`)
ae_p <- ae_p %>% 
  dplyr::rename(p.value=`ae$p.value`)
af_p <- af_p %>% 
  dplyr::rename(p.value=`af$p.value`)
ag_p <- ag_p %>% 
  dplyr::rename(p.value=`ag$p.value`)
names(ab_p)
pval <- rbind(a_p, ab_p,ac_p, ad_p,ae_p,af_p,ag_p)



median_overall <- merge(wk_med, pval, by="lock")


setwd("~/CCU007_01/results")

date <-  '15_05_2023'
write_csv(median_overall,paste0("Overall_median_",date, ".csv"))

# Figure 3: Activity group  mean difference % -----


data_final <- data_final %>% 
  dplyr::filter(actv_cat!=5)


data_final <- data_final %>% 
  dplyr::mutate(actv_gp=ifelse(actv_cat==1,1,
                               ifelse(actv_cat==2,2,3)))


d1 <- data_final %>% 
  dplyr::filter(lock==1) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock1=n()) %>% 
  dplyr::mutate(total1=sum(lock1)) 


d2 <- data_final %>% 
  dplyr::filter(lock==2) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock2=n()) %>% 
  dplyr::mutate(total2=sum(lock2)) 

d3 <- data_final %>% 
  dplyr::filter(lock==3) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock3=n()) %>% 
  dplyr::mutate(total3=sum(lock3)) 

d4 <- data_final %>% 
  dplyr::filter(lock==4) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock4=n()) %>% 
  dplyr::mutate(total4=sum(lock4)) 

d5 <- data_final %>% 
  dplyr::filter(lock==5) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock5=n()) %>% 
  dplyr::mutate(total5=sum(lock5)) 

d6 <- data_final %>% 
  dplyr::filter(lock==6) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock6=n()) %>% 
  dplyr::mutate(total6=sum(lock6)) 

d7 <- data_final %>% 
  dplyr::filter(lock==7) %>% 
  dplyr::group_by(actv_gp) %>% 
  dplyr::summarise(lock7=n()) %>% 
  dplyr::mutate(total7=sum(lock7)) 

d12 <- merge(d1,d2, by="actv_gp", all.x = TRUE)
d123 <- merge(d12,d3, by="actv_gp", all.x = TRUE)
d1234 <- merge(d123,d4, by="actv_gp", all.x = TRUE)
d12345 <- merge(d1234,d5, by="actv_gp", all.x = TRUE)
d123456 <- merge(d12345,d6, by="actv_gp", all.x = TRUE)
d1234567 <- merge(d123456,d7, by="actv_gp", all.x = TRUE)

setwd("~/CCU007_01/results")


write_csv(d1234567, paste0("ccu007_01_proc_rate_actv_gp.csv"))

proc_rate <- read_csv("ccu007_01_proc_rate_actv_gp.csv")

rate_wide <- proc_rate %>% 
  dplyr::mutate(perc1=(lock1/total1)*100,
                perc2=(lock2/total2)*100,
                perc3=(lock3/total3)*100,
                perc4=(lock4/total4)*100,
                perc5=(lock5/total5)*100,
                perc6=(lock6/total6)*100,
                perc7=(lock7/total7)*100) %>% 
  dplyr::mutate(aplha=0.05,
                z=qnorm(1-aplha/2),
                lci1=perc1-z*sqrt(perc1*(100-perc1)/total1),
                uci1=perc1+z*sqrt(perc1*(100-perc1)/total1),
                lci2=perc2-z*sqrt(perc2*(100-perc2)/total2),
                uci2=perc2+z*sqrt(perc2*(100-perc2)/total2),
                lci3=perc3-z*sqrt(perc3*(100-perc3)/total3),
                uci3=perc3+z*sqrt(perc3*(100-perc3)/total3),
                lci4=perc4-z*sqrt(perc4*(100-perc4)/total4),
                uci4=perc4+z*sqrt(perc4*(100-perc4)/total4),
                lci5=perc5-z*sqrt(perc5*(100-perc5)/total5),
                uci5=perc5+z*sqrt(perc5*(100-perc5)/total5),
                lci6=perc6-z*sqrt(perc6*(100-perc6)/total6),
                uci6=perc6+z*sqrt(perc6*(100-perc6)/total6),
                lci7=perc7-z*sqrt(perc7*(100-perc7)/total7),
                uci7=perc7+z*sqrt(perc7*(100-perc7)/total7)
  )
rate_wide1 <- rate_wide %>% 
  dplyr::mutate(diff2=perc2-perc1,
                diff3=perc3-perc1,
                diff4=perc4-perc1,
                diff5=perc5-perc1,
                diff6=perc6-perc1,
                diff7=perc7-perc1,
                diff1=perc1) %>% 
  dplyr::mutate(aplha=0.05,
                z=qnorm(1-aplha/2),
                dlci2=diff2-z*sqrt((perc1*(100-perc1)/total1)+(perc2*(100-perc2)/total2)),
                duci2=diff2+z*sqrt((perc1*(100-perc1)/total1)+(perc2*(100-perc2)/total2)),
                dlci3=diff3-z*sqrt((perc1*(100-perc1)/total1)+(perc3*(100-perc3)/total3)),
                duci3=diff3+z*sqrt((perc1*(100-perc1)/total1)+(perc3*(100-perc3)/total3)),
                dlci4=diff4-z*sqrt((perc1*(100-perc1)/total1)+(perc4*(100-perc4)/total4)),
                duci4=diff4+z*sqrt((perc1*(100-perc1)/total1)+(perc4*(100-perc4)/total4)),
                dlci5=diff5-z*sqrt((perc1*(100-perc1)/total1)+(perc5*(100-perc5)/total5)),
                duci5=diff5+z*sqrt((perc1*(100-perc1)/total1)+(perc5*(100-perc5)/total5)),
                dlci6=diff6-z*sqrt((perc1*(100-perc1)/total1)+(perc6*(100-perc6)/total6)),
                duci6=diff6+z*sqrt((perc1*(100-perc1)/total1)+(perc6*(100-perc6)/total6)),
                dlci7=diff7-z*sqrt((perc1*(100-perc1)/total1)+(perc7*(100-perc7)/total7)),
                duci7=diff7+z*sqrt((perc1*(100-perc1)/total1)+(perc7*(100-perc7)/total7))
                
  ) %>% 
  dplyr::select(actv_gp, diff1, diff2,diff3, diff4, diff5, diff6, diff7,dlci2, duci2,dlci3, duci3,dlci4, duci4,dlci5, duci5,dlci6, duci6,dlci7, duci7)


rate_wide1 <- as.data.frame(rate_wide1)
rate_diff <- reshape(rate_wide1, direction = "long",
                     varying=c('diff2','dlci2','duci2','diff3','dlci3','duci3','diff4','dlci4','duci4','diff5','dlci5','duci5','diff6','dlci6','duci6','diff7','dlci7','duci7'),
                     timevar='lock',
                     times=c('2','3','4','5','6','7'),
                     v.names = c('dlci','diff','duci'),
                     idvar='actv_gp')



write_csv(rate_diff, paste0("ccu007_01_lockdown_proc_ratediff_actv_gp.csv"))


# Figure 4 Age group mean differnce % -----------

data_final <- data_final %>% 
  dplyr::filter(actv_cat!=5)


data_final <- data_final %>% 
  dplyr::mutate(dob=as.Date(`DATE_OF_BIRTH`),
                age_years=(visit_dt-dob)/365.25)
data_final$age_years <- as.numeric(data_final$age_years)

data_final$age_yr <- abs(round(data_final$age_years, digits = 3))

data_final <- data_final %>% 
  dplyr::mutate(aged=age_yr*365.25,
                agegp=ifelse(aged<365.25, 1,
                             ifelse(aged>=365.25 & aged<1826.25,2,
                                    ifelse(aged>=1826.25 & aged<3652.5,3,
                                           ifelse(aged>=3652.5 & !is.na(aged),4, NA)))))

                               


d1 <- data_final %>% 
  dplyr::filter(lock==1) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock1=n()) %>% 
  dplyr::mutate(total1=sum(lock1)) 


d2 <- data_final %>% 
  dplyr::filter(lock==2) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock2=n()) %>% 
  dplyr::mutate(total2=sum(lock2)) 

d3 <- data_final %>% 
  dplyr::filter(lock==3) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock3=n()) %>% 
  dplyr::mutate(total3=sum(lock3)) 

d4 <- data_final %>% 
  dplyr::filter(lock==4) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock4=n()) %>% 
  dplyr::mutate(total4=sum(lock4)) 

d5 <- data_final %>% 
  dplyr::filter(lock==5) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock5=n()) %>% 
  dplyr::mutate(total5=sum(lock5)) 

d6 <- data_final %>% 
  dplyr::filter(lock==6) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock6=n()) %>% 
  dplyr::mutate(total6=sum(lock6)) 

d7 <- data_final %>% 
  dplyr::filter(lock==7) %>% 
  dplyr::group_by(agegp) %>% 
  dplyr::summarise(lock7=n()) %>% 
  dplyr::mutate(total7=sum(lock7)) 

d12 <- merge(d1,d2, by="agegp", all.x = TRUE)
d123 <- merge(d12,d3, by="agegp", all.x = TRUE)
d1234 <- merge(d123,d4, by="agegp", all.x = TRUE)
d12345 <- merge(d1234,d5, by="agegp", all.x = TRUE)
d123456 <- merge(d12345,d6, by="agegp", all.x = TRUE)
d1234567 <- merge(d123456,d7, by="agegp", all.x = TRUE)

setwd("~/CCU007_01/results")


write_csv(d1234567, paste0("ccu007_01_proc_rate_agegp.csv"))

proc_rate <- read_csv("ccu007_01_proc_rate_agegp.csv")

rate_wide <- proc_rate %>% 
  dplyr::mutate(perc1=(lock1/total1)*100,
                perc2=(lock2/total2)*100,
                perc3=(lock3/total3)*100,
                perc4=(lock4/total4)*100,
                perc5=(lock5/total5)*100,
                perc6=(lock6/total6)*100,
                perc7=(lock7/total7)*100) %>% 
  dplyr::mutate(aplha=0.05,
                z=qnorm(1-aplha/2),
                lci1=perc1-z*sqrt(perc1*(100-perc1)/total1),
                uci1=perc1+z*sqrt(perc1*(100-perc1)/total1),
                lci2=perc2-z*sqrt(perc2*(100-perc2)/total2),
                uci2=perc2+z*sqrt(perc2*(100-perc2)/total2),
                lci3=perc3-z*sqrt(perc3*(100-perc3)/total3),
                uci3=perc3+z*sqrt(perc3*(100-perc3)/total3),
                lci4=perc4-z*sqrt(perc4*(100-perc4)/total4),
                uci4=perc4+z*sqrt(perc4*(100-perc4)/total4),
                lci5=perc5-z*sqrt(perc5*(100-perc5)/total5),
                uci5=perc5+z*sqrt(perc5*(100-perc5)/total5),
                lci6=perc6-z*sqrt(perc6*(100-perc6)/total6),
                uci6=perc6+z*sqrt(perc6*(100-perc6)/total6),
                lci7=perc7-z*sqrt(perc7*(100-perc7)/total7),
                uci7=perc7+z*sqrt(perc7*(100-perc7)/total7)
  )
rate_wide1 <- rate_wide %>% 
  dplyr::mutate(diff2=perc2-perc1,
                diff3=perc3-perc1,
                diff4=perc4-perc1,
                diff5=perc5-perc1,
                diff6=perc6-perc1,
                diff7=perc7-perc1,
                diff1=perc1) %>% 
  dplyr::mutate(aplha=0.05,
                z=qnorm(1-aplha/2),
                dlci2=diff2-z*sqrt((perc1*(100-perc1)/total1)+(perc2*(100-perc2)/total2)),
                duci2=diff2+z*sqrt((perc1*(100-perc1)/total1)+(perc2*(100-perc2)/total2)),
                dlci3=diff3-z*sqrt((perc1*(100-perc1)/total1)+(perc3*(100-perc3)/total3)),
                duci3=diff3+z*sqrt((perc1*(100-perc1)/total1)+(perc3*(100-perc3)/total3)),
                dlci4=diff4-z*sqrt((perc1*(100-perc1)/total1)+(perc4*(100-perc4)/total4)),
                duci4=diff4+z*sqrt((perc1*(100-perc1)/total1)+(perc4*(100-perc4)/total4)),
                dlci5=diff5-z*sqrt((perc1*(100-perc1)/total1)+(perc5*(100-perc5)/total5)),
                duci5=diff5+z*sqrt((perc1*(100-perc1)/total1)+(perc5*(100-perc5)/total5)),
                dlci6=diff6-z*sqrt((perc1*(100-perc1)/total1)+(perc6*(100-perc6)/total6)),
                duci6=diff6+z*sqrt((perc1*(100-perc1)/total1)+(perc6*(100-perc6)/total6)),
                dlci7=diff7-z*sqrt((perc1*(100-perc1)/total1)+(perc7*(100-perc7)/total7)),
                duci7=diff7+z*sqrt((perc1*(100-perc1)/total1)+(perc7*(100-perc7)/total7))
                
  ) %>% 
  dplyr::select(agegp, diff1, diff2,diff3, diff4, diff5, diff6, diff7,dlci2, duci2,dlci3, duci3,dlci4, duci4,dlci5, duci5,dlci6, duci6,dlci7, duci7)


rate_wide1 <- as.data.frame(rate_wide1)
rate_diff <- reshape(rate_wide1, direction = "long",
                     varying=c('diff2','dlci2','duci2','diff3','dlci3','duci3','diff4','dlci4','duci4','diff5','dlci5','duci5','diff6','dlci6','duci6','diff7','dlci7','duci7'),
                     timevar='lock',
                     times=c('2','3','4','5','6','7'),
                     v.names = c('dlci','diff','duci'),
                     idvar='agegp')



write_csv(rate_diff, paste0("ccu007_01_lockdown_proc_ratediff_agegp.csv"))



# Supplementary Figure S1: Urgency mean difference % -----------


data_final <- data_final %>% 
  dplyr::filter(actv_cat!=5)


data_final <- data_final %>% 
  dplyr::mutate(urg_cat=ifelse(`3_01B_PROCEDURE_URGENCY`=="1. Elective", 0, 
                               ifelse(`3_01B_PROCEDURE_URGENCY`=="2. Urgent",1,
                                      ifelse(!is.na(`3_01B_PROCEDURE_URGENCY`),2,NA))))



                               


d1 <- data_final %>% 
  dplyr::filter(lock==1) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock1=n()) %>% 
  dplyr::mutate(total1=sum(lock1)) 


d2 <- data_final %>% 
  dplyr::filter(lock==2) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock2=n()) %>% 
  dplyr::mutate(total2=sum(lock2)) 

d3 <- data_final %>% 
  dplyr::filter(lock==3) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock3=n()) %>% 
  dplyr::mutate(total3=sum(lock3)) 

d4 <- data_final %>% 
  dplyr::filter(lock==4) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock4=n()) %>% 
  dplyr::mutate(total4=sum(lock4)) 

d5 <- data_final %>% 
  dplyr::filter(lock==5) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock5=n()) %>% 
  dplyr::mutate(total5=sum(lock5)) 

d6 <- data_final %>% 
  dplyr::filter(lock==6) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock6=n()) %>% 
  dplyr::mutate(total6=sum(lock6)) 

d7 <- data_final %>% 
  dplyr::filter(lock==7) %>% 
  dplyr::group_by(urg_cat) %>% 
  dplyr::summarise(lock7=n()) %>% 
  dplyr::mutate(total7=sum(lock7)) 

d12 <- merge(d1,d2, by="urg_cat", all.x = TRUE)
d123 <- merge(d12,d3, by="urg_cat", all.x = TRUE)
d1234 <- merge(d123,d4, by="urg_cat", all.x = TRUE)
d12345 <- merge(d1234,d5, by="urg_cat", all.x = TRUE)
d123456 <- merge(d12345,d6, by="urg_cat", all.x = TRUE)
d1234567 <- merge(d123456,d7, by="urg_cat", all.x = TRUE)

setwd("~/CCU007_01/results")


write_csv(d1234567, paste0("ccu007_01_proc_rate_urg_cat.csv"))

proc_rate <- read_csv("ccu007_01_proc_rate_urg_cat.csv")

rate_wide <- proc_rate %>% 
  dplyr::mutate(perc1=(lock1/total1)*100,
                perc2=(lock2/total2)*100,
                perc3=(lock3/total3)*100,
                perc4=(lock4/total4)*100,
                perc5=(lock5/total5)*100,
                perc6=(lock6/total6)*100,
                perc7=(lock7/total7)*100) %>% 
  dplyr::mutate(aplha=0.05,
                z=qnorm(1-aplha/2),
                lci1=perc1-z*sqrt(perc1*(100-perc1)/total1),
                uci1=perc1+z*sqrt(perc1*(100-perc1)/total1),
                lci2=perc2-z*sqrt(perc2*(100-perc2)/total2),
                uci2=perc2+z*sqrt(perc2*(100-perc2)/total2),
                lci3=perc3-z*sqrt(perc3*(100-perc3)/total3),
                uci3=perc3+z*sqrt(perc3*(100-perc3)/total3),
                lci4=perc4-z*sqrt(perc4*(100-perc4)/total4),
                uci4=perc4+z*sqrt(perc4*(100-perc4)/total4),
                lci5=perc5-z*sqrt(perc5*(100-perc5)/total5),
                uci5=perc5+z*sqrt(perc5*(100-perc5)/total5),
                lci6=perc6-z*sqrt(perc6*(100-perc6)/total6),
                uci6=perc6+z*sqrt(perc6*(100-perc6)/total6),
                lci7=perc7-z*sqrt(perc7*(100-perc7)/total7),
                uci7=perc7+z*sqrt(perc7*(100-perc7)/total7)
  )
rate_wide1 <- rate_wide %>% 
  dplyr::mutate(diff2=perc2-perc1,
                diff3=perc3-perc1,
                diff4=perc4-perc1,
                diff5=perc5-perc1,
                diff6=perc6-perc1,
                diff7=perc7-perc1,
                diff1=perc1) %>% 
  dplyr::mutate(aplha=0.05,
                z=qnorm(1-aplha/2),
                dlci2=diff2-z*sqrt((perc1*(100-perc1)/total1)+(perc2*(100-perc2)/total2)),
                duci2=diff2+z*sqrt((perc1*(100-perc1)/total1)+(perc2*(100-perc2)/total2)),
                dlci3=diff3-z*sqrt((perc1*(100-perc1)/total1)+(perc3*(100-perc3)/total3)),
                duci3=diff3+z*sqrt((perc1*(100-perc1)/total1)+(perc3*(100-perc3)/total3)),
                dlci4=diff4-z*sqrt((perc1*(100-perc1)/total1)+(perc4*(100-perc4)/total4)),
                duci4=diff4+z*sqrt((perc1*(100-perc1)/total1)+(perc4*(100-perc4)/total4)),
                dlci5=diff5-z*sqrt((perc1*(100-perc1)/total1)+(perc5*(100-perc5)/total5)),
                duci5=diff5+z*sqrt((perc1*(100-perc1)/total1)+(perc5*(100-perc5)/total5)),
                dlci6=diff6-z*sqrt((perc1*(100-perc1)/total1)+(perc6*(100-perc6)/total6)),
                duci6=diff6+z*sqrt((perc1*(100-perc1)/total1)+(perc6*(100-perc6)/total6)),
                dlci7=diff7-z*sqrt((perc1*(100-perc1)/total1)+(perc7*(100-perc7)/total7)),
                duci7=diff7+z*sqrt((perc1*(100-perc1)/total1)+(perc7*(100-perc7)/total7))
                
  ) %>% 
  dplyr::select(urg_cat, diff1, diff2,diff3, diff4, diff5, diff6, diff7,dlci2, duci2,dlci3, duci3,dlci4, duci4,dlci5, duci5,dlci6, duci6,dlci7, duci7)


rate_wide1 <- as.data.frame(rate_wide1)
rate_diff <- reshape(rate_wide1, direction = "long",
                     varying=c('diff2','dlci2','duci2','diff3','dlci3','duci3','diff4','dlci4','duci4','diff5','dlci5','duci5','diff6','dlci6','duci6','diff7','dlci7','duci7'),
                     timevar='lock',
                     times=c('2','3','4','5','6','7'),
                     v.names = c('dlci','diff','duci'),
                     idvar='urg_cat')



write_csv(rate_diff, paste0("ccu007_01_lockdown_proc_ratediff_urg_cat.csv"))