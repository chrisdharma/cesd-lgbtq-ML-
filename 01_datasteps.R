#load data
library(sas7bdat)
library(tableone)
library(naniar)
library(ggplot2)
library(tidyr)
library(caret)
library(randomForest)
library(mice)
library(vip)
library(corrplot)
library(pdp)
library(dplyr)

setwd("/Users/christofferdharma/Documents/CAMH-Chaiton")
datold<-read.sas7bdat("baselinefup1.sas7bdat")

str(datold)
dim(datold)
#1511 observations and 506 variables

#Select participants who completed follow up survey, 
##just first removing those who followup_complete1 is missing (which seems all NA)
dat<- datold[!is.na(datold$followup1_complete),]
dat<- dat[dat$baseline_complete==2,]
#they have to complete both!

dim(dat)

table(dat$substances___12)
table(dat$substances_1___12)

CreateTableOne(strata=c("substances___12"),vars=c("substances_1___12"),factorVars = c("substances_1___12"),data = dat)

CreateTableOne(strata=c("diagnosis"),vars=c("diagnosis_1"),factorVars = c("diagnosis_1"),data = dat)

# dat2 <- dat %>% select(substances_1___12, substances___12)

#Stratified by those who seek mental health services

dat <- dat %>% mutate(select(.,c(feel1:feel7,feel1_1:feel7_1)) %>% replace_with_na_all(condition = ~.x == 5))

count_na_func <- function(x) sum(is.na(x)) 

dat <- dat %>%
  mutate(sum_na_cesd = select(.,feel1:feel7) %>% apply(1, count_na_func),
         sum_na_cesd1 = select(.,feel1_1:feel7_1) %>% apply(1, count_na_func))

dat<- dat %>% mutate(cesd_score = select(.,feel1:feel7) %>% rowMeans(na.rm=TRUE),
                     cesd1_score = select(.,feel1_1:feel7_1) %>% rowMeans(na.rm=TRUE))

dat <- dat %>% mutate(cesd_score = ifelse(sum_na_cesd >= 2,NA,cesd_score),
                      cesd1_score = ifelse(sum_na_cesd1 >= 2,NA,cesd1_score),
                      diff_cesd = cesd1_score - cesd_score)

#Since we've removed NAs, if they had 0, it will be NA (i.e. those whose scores are all NAs)
#Lowest is 1, so it cannot be all 0
#if missing more than 10% (i.e more than 1 item), then scores cannot be used

# check<-dat %>% filter(cesd_score ==4) %>% select(.,c(feel1:feel7,feel1_1:feel7_1))

plot(cesd1_score ~ cesd_score,data=dat)

# x <- dat %>% select(.,cesd_score,cesd1_score)
# x <- na.omit(x)

# tot.sse <- c()
# 
# for (i in 1:5){
#   km.out <- kmeans(x,i,nstart=1)
#   tot.sse[i] <- km.out$tot.withinss
# }
# plot(tot.sse,type="l")
# 
# #Maybe 2/3 is a good fit?
# km.out2 <- kmeans(x,2,nstart=1)
# km.out3 <- kmeans(x,3,nstart=1)
# 
# plot(x,col=4-km.out2$cluster)
# plot(x,col=4-km.out3$cluster)
#Not that useful, because the clusters only show those with low consistently low, mid consistently mid, etc
#So possibly still best to use LASSO to select the variables or random forest, and read those PDP paper

dat %>% summarise(n=n(),
                  mean_cesd=mean(cesd_score,na.rm=TRUE),
                  mean_cesd1=mean(cesd1_score,na.rm=TRUE),
                  median_cesd=median(cesd_score,na.rm=TRUE),
                  median_cesd1=median(cesd1_score,na.rm=TRUE),
                  min_cesd=min(cesd_score,na.rm=TRUE),
                  min_cesd1=min(cesd1_score,na.rm=TRUE),
                  max_cesd=max(cesd_score,na.rm=TRUE),
                  max_cesd1=max(cesd1_score,na.rm=TRUE))

##https://www.listendata.com/2014/11/random-forest-with-r.html#id-1ed30f.
#It doesnt make sense to predict them with a binary classifier, so we can do continuous outcome (methods such as RF)

t.test(dat$cesd_score, dat$cesd1_score, paired = TRUE, alternative = "two.sided")
###No significant difference between the two, which makes sense but the patterns of changes should be noted

# t.test(dat2$diff_cesd ~ dat2$seek_help, var.equal=TRUE)

#It does appear those who seek help have higher improvements (lower CESD scores) 
#although CESD scores do get higher for some
##Is this an effect modifier or just another correlate? Should we look at them separately?

# Generally cut-off >= 8 / 9 has been used in other studies.
# But this is really low! We can keep it continuous and do some type of latent trajectory analysis
# or k-means?

CreateTableOne(vars = c("followup1_complete"), data = datold, factorVars = c("followup1_complete"))
#Only 926 completed follow up, I suppose the missing means they did not show up?

#no NA values or no responses for don't know or prefer not to answer

#I would change the grouping as:
# 0 = Asexual
# 1 = straight, heterosexual or heteroflexible
# 2 = bisexual
# 3 = gay
# 4 = lesbian
# 5 = pansexual
# 6 = queer
# 7 = questioning
# 8 = Two spirit

dat$curr_orient2 <-
  case_when(
    dat$curr_orient == 1 ~ 0,
    dat$curr_orient  %in% c(4,9) ~ 1,
    dat$curr_orient  == 2 ~ 2,
    dat$curr_orient  == 3 ~ 3,
    dat$curr_orient  == 5 ~ 4,
    dat$curr_orient  == 6 ~ 5,
    dat$curr_orient  == 7 ~ 6,
    dat$curr_orient  == 8 ~ 7,
    dat$curr_orient  == 10 ~ 8)

#creating new variable gender
#0-> cisgender men
#1-> cisgender women
#2-> transgender men
#3-> transgender women
#4 -> gender diverse
dat$gender<- NA
0 -> dat$gender[which(dat$curr_gender==5 & dat$trans==2)]
1 -> dat$gender[which(dat$curr_gender==6 & dat$trans==2)]
2 -> dat$gender[which(dat$curr_gender==5 & dat$trans==1)]
3 -> dat$gender[which(dat$curr_gender==6 & dat$trans==1)]
4 -> dat$gender[which(dat$curr_gender==1 | dat$curr_gender==2 |dat$curr_gender==3 |dat$curr_gender==4 | dat$curr_gender==7)]
NA -> dat$gender[which(dat$curr_gender==8 | dat$curr_gender==9)]
dat$gender <- as.factor(dat$gender)
table(dat$gender)

#trans, intersex, poc
table(dat$trans)
NA -> dat$trans[which(dat$trans==3 | dat$trans==4)]
0 -> dat$trans[which(dat$trans==2)]
table(dat$intersex)
NA -> dat$intersex[which(dat$intersex==3 | dat$intersex==4)]
0 -> dat$intersex[which(dat$intersex==2)]
table(dat$poc)
NA -> dat$poc[which(dat$poc==3)]
0 -> dat$poc[which(dat$poc==2)]
# 
# 0 -> dat$suicidal[which(dat$suicidal==1)]
# 1 -> dat$suicidal[which(dat$suicidal==2 | dat$suicidal==3 | dat$suicidal==4 | dat$suicidal==5)]
NA -> dat$suicidal[which(dat$suicidal==6)]
# dat$suicidal <- as.factor(dat$suicidal)

#seek_help
table(dat$seek_help)
0 -> dat$seek_help[which(dat$seek_help== 7)]
1 -> dat$seek_help[which(dat$seek_help== 6)]
NA -> dat$seek_help[which(dat$seek_help== 8 | dat$seek_help==9)]

#N=1089 for help_delay (dat$seek_help==1)
table(dat$help_delay)
round(100*prop.table(table(dat$help_delay)),digits=0)
0 -> dat$help_delay[which(dat$help_delay==2)]
1 -> dat$help_delay[which(dat$help_delay==1)]
2 -> dat$help_delay[which(is.na(dat$help_delay))]
NA -> dat$help_delay[which(dat$help_delay==3 | dat$help_delay==4)]
dat$help_delay <- as.factor(dat$help_delay)

#lifetime smoking and current smoking status
table(dat$cigs_smoked)
# 0 -> dat$cigs_smoked[which(dat$cigs_smoked==1 | dat$cigs_smoked==2 | dat$cigs_smoked==3 | dat$cigs_smoked==4 | dat$cigs_smoked==5 | dat$cigs_smoked==6)]
# 1 -> dat$cigs_smoked[which(dat$cigs_smoked==7)]
NA -> dat$cigs_smoked[which(dat$cigs_smoked==8)]
# dat$cigs_smoked <- as.factor(dat$cigs_smoked)

table(dat$curr_smoke,useNA="ifany")
# 0 -> dat$curr_smoke[which(is.na(dat$curr_smoke))]
# 2 -> dat$curr_smoke[which(dat$curr_smoke==2 | dat$curr_smoke==3)]
# 3 -> dat$curr_smoke[which(dat$curr_smoke==4 | dat$curr_smoke==5)]
# dat$curr_smoke <- as.factor(dat$curr_smoke)

#dropping smoking variables not needed
dat <- dat %>% select(-c(last_smoked: brand)) %>% select(-c(reason___1: cues___22))
dat$friends_smoke <- NULL
dat <- dat %>% select(-c(why_stop___1: why_stop___11)) %>% select(-c(time_quit : cess_barriers___16))
dat <- dat %>% select(-c(sprtgrp:smok_drg))
dat <- dat %>% select(-c(quitimport1:health_change))

#recoding few smoking variables
table(dat$covid)
0 -> dat$covid[which(is.na(dat$covid))]
dat$covid <- as.factor(dat$covid)

#making a new variable, dat$ethnicity and collapsing other vaiables of ethnicity

dat$ethnicity <- NA

#Start with white
0 -> dat$ethnicity[which(dat$ethnicity___13==1 | dat$ethnicity___14==1)]

#If picked something else, these would be replaced by the following

#Latin American
1 -> dat$ethnicity[which(dat$ethnicity___10==1)]

#Middle Eastern
2 -> dat$ethnicity[which(dat$ethnicity___12==1)]

#East and Southeast Asian
3 -> dat$ethnicity[which(dat$ethnicity___1==1 | dat$ethnicity___3==1)]

#South Asian
4 -> dat$ethnicity[which(dat$ethnicity___2==1)]

#Black
5 -> dat$ethnicity[which(dat$ethnicity___4==1 | dat$ethnicity___5==1 | dat$ethnicity___6==1)]

#Indigenous / Metis
6 -> dat$ethnicity[which(dat$ethnicity___7==1 | dat$ethnicity___8==1 | dat$ethnicity___9==1 | dat$ethnicity___11==1)]

#Mixed
7 -> dat$ethnicity[which(dat$ethnicity___15==1)]

NA -> dat$ethnicity[which(dat$ethnicity___16==1 | dat$ethnicity___17==1)]

dat <- dat %>% mutate(ethnicity1 = select(.,ethnicity___1:ethnicity___14) %>% rowSums(na.rm=TRUE))
#A few of them have more than 1 ethnicity

datcheck<-subset(dat,ethnicity1 >= 2)
datcheck<-datcheck %>%  select(.,ethnicity___1:ethnicity___15) 
table(datcheck$ethnicity___15)
#Many did not pick mixed even though they selected more than one race.

#recoding risk perception of smoking and vaping
# (risk1 -- risk4, probably keep it as 1 - 5, don't see why we should recode)
# (Opinion variables:
# rates, accept, stress, stigma, pressure, mhealth, culture, can be kept as 5 levels still)

#recoding smoking cessation variables
table(dat$plan_quit)
11 ->dat$plan_quit[which(dat$plan_quit==1)]
1 -> dat$plan_quit[which(dat$plan_quit==2 | dat$plan_quit==3 | dat$plan_quit==4)]
0 -> dat$plan_quit[which(dat$plan_quit==5)]
2 -> dat$plan_quit[which(dat$plan_quit==11)]
3 -> dat$plan_quit[which(is.na(dat$plan_quit))]
dat$plan_quit <- as.factor(dat$plan_quit)

table(dat$quit_attempts)
0 -> dat$quit_attempts[which(dat$quit_attempts==1)]
1 -> dat$quit_attempts[which(dat$quit_attempts==2 | dat$quit_attempts==3 | dat$quit_attempts==4)]
2 -> dat$quit_attempts[which(is.na(dat$quit_attempts))]
dat$quit_attempts <- as.factor(dat$quit_attempts)

table(dat$tailored)
0-> dat$tailored[which(dat$tailored==2)]
2-> dat$tailored[which(dat$tailored==3)]
3 -> dat$tailored[which(is.na(dat$tailored))]
#NA values are kept as seperate level 3 here
dat$tailored <- as.factor(dat$tailored)

summary(dat$quit_support)
table(dat$quit_support)
0 -> dat$quit_support[which(dat$quit_support==1 | dat$quit_support==2)]
2 -> dat$quit_support[which(dat$quit_support==3)]
1 -> dat$quit_support[which(dat$quit_support==4 | dat$quit_support==5)]
3 -> dat$quit_support[which(is.na(dat$quit_support))]
dat$quit_support <- as.factor(dat$quit_support)

##vaping variables
#lifetime and current vaping frequency
0 -> dat$time_vape[which(dat$time_vape<=6)]
1 -> dat$time_vape[which(dat$time_vape==7)]
NA -> dat$time_vape[which(dat$time_vape==8)]
dat$time_vape <- as.factor(dat$time_vape)

0-> dat$curr_vape[which(is.na(dat$curr_vape))]
1 -> dat$curr_vape[which(dat$curr_vape==1 | dat$curr_vape==2 | dat$curr_vape==3)]
2 -> dat$curr_vape[which(dat$curr_vape==4 | dat$curr_vape==5)]

#dropping vaping variables not needed
dat <- dat %>% select(-c(reasons_vape___1:vape_accept_friends))

##alcohol
table(dat$alcohol)
1 -> dat$alcohol[which(dat$alcohol==1 | dat$alcohol==2 | dat$alcohol ==3 | dat$alcohol==4)]
2 -> dat$alcohol[which(dat$alcohol==5 | dat$alcohol==6 | dat$alcohol==7)]
0 -> dat$alcohol[which(dat$alcohol==8)]
dat$alcohol <- as.factor((dat$alcohol))

0 -> dat$alcohol_amount[which(dat$alcohol_amount==1)]
1 -> dat$alcohol_amount[which(dat$alcohol_amount>=2)]
2 -> dat$alcohol_amount[which(is.na(dat$alcohol_amount))]
dat$alcohol_amount <- as.factor(dat$alcohol_amount)

##cannabis
0 -> dat$cannabis[which(dat$cannabis==8)]
1 -> dat$cannabis[which(dat$cannabis>=1 & dat$cannabis<5)]
2 -> dat$cannabis[which(dat$cannabis==5 | dat$cannabis==6 | dat$cannabis==7)]
dat$cannabis <- as.factor(dat$cannabis)

##substance use
#creating new variable for past 12 month drug use
dat$drug_12m <- 0
1 -> dat$drug_12m[which(dat$substances___13==0)]
NA-> dat$drug_12m[which(dat$substances___14==1)]
dat$drug_12m <- as.factor(dat$drug_12m)

#renaming other drug use variables
1 -> dat$drug_12m[which(dat$substances___1==1 | dat$substances___2==1 | dat$substances___3==1 | 
                          dat$substances___4==1 | dat$substances___5==1 | dat$substances___6==1 |
                          dat$substances___7==1 | dat$substances___8==1 | dat$substances___9==1 | 
                          dat$substances___10==1 | dat$substances___11==1 | dat$substances___12==1 | 
                          dat$substances___15==1)]
NA-> dat$drug_12m[which(dat$substances___14==1)]
dat$drug_12m <- as.factor(dat$drug_12m)

dat <- dat %>% rename(poppers = substances___1, special_K = substances___2, 
                      MDMA= substances___3, crystal_meth=substances___4, crack=substances___5,
                      cocaine=substances___6, heroin=substances___7,pres_opioids=substances___8,
                      fentanyl=substances___9, GHB=substances___10, tranquilizers=substances___11,
                      psychedelics=substances___12,drug_others=substances___15)
dat$substances___13 <- NULL
dat$substances___14 <- NULL

#covid-19 impact on substance use
0 -> dat$substances_covid[which(dat$substances_covid==5)]
dat$substances_covid <- as.factor(dat$substances_covid)
dat$substances_covid[46] <- NA

##recoding health variables
table(dat$gen_health)
NA -> dat$gen_health[which(dat$gen_health==6 | dat$gen_health==7)]
# 0 -> dat$gen_health[which(dat$gen_health==1 | dat$gen_health==2)]
# 1 -> dat$gen_health[which(dat$gen_health==3 | dat$gen_health==4 | dat$gen_health==5)]
# dat$gen_health <- as.factor(dat$gen_health)

table(dat$fitness1)
NA -> dat$fitness1[which(dat$fitness1==3)]
0 -> dat$fitness1[which(dat$fitness1==2)]

table(dat$mental_health)
NA -> dat$mental_health[which(dat$mental_health==6 | dat$mental_health==7)]
# 0 -> dat$mental_health[which(dat$mental_health==1 | dat$mental_health==2)]
# 1 -> dat$mental_health[which(dat$mental_health==3 | dat$mental_health==4 | dat$mental_health==5)]
# dat$mental_health <- as.factor(dat$mental_health)

table(dat$stresslife)
dat$stresslife <- 6 - dat$stresslife
# table(dat$stresslife)
# 0 -> dat$stresslife[which(dat$stresslife==3 | dat$stresslife==4 | dat$stresslife==5)]
# 1 -> dat$stresslife[which(dat$stresslife==1 | dat$stresslife==2)]
# dat$stresslife <- as.factor(dat$stresslife)

table(dat$suicidal)
# 0 -> dat$suicidal[which(dat$suicidal==1)]
# 1 -> dat$suicidal[which(dat$suicidal==2 | dat$suicidal==3 | dat$suicidal==4 | dat$suicidal==5)]
NA -> dat$suicidal[which(dat$suicidal==6)]
# dat$suicidal <- as.factor(dat$suicidal)

table(dat$diagnosis)
0 -> dat$diagnosis[which(dat$diagnosis==2)]
NA -> dat$diagnosis[which(dat$diagnosis==3 | dat$diagnosis==4)]

0 -> dat$diagnosis_1[which(dat$diagnosis_1==2)]
NA -> dat$diagnosis_1[which(dat$diagnosis_1==3 | dat$diagnosis_1==4)]

#mental health conditions
dat <- rename(dat, con_eating = conditions___1, con_anxiety= conditions___2,
              con_ADD = conditions___3, con_ADHD = conditions___4, con_bipolar=
                conditions___5, con_depression= conditions___6, con_OCD=conditions___9,
              con_panic= conditions___10, con_PTSD= conditions___13, con_others= conditions___15)

#merging mental health variables with <5% positive response to others. These are the ones
#where there is very little responses (such as scizophrenia, where there were none)
1-> dat$con_others[which(dat$con_others==1 | dat$conditions___7==1 | dat$conditions___8==1
                         | dat$conditions___11==1 | dat$conditions___12==1 | dat$conditions___14==1)]
dat <- dat %>% select(-c(conditions___7, conditions___8, conditions___11, conditions___12, 
                         conditions___14, conditions___16, conditions___17))

#comorbid conditions
dat$treat_comorbid <- 0
1 -> dat$treat_comorbid[which(dat$treatment___1==1 | dat$treatment___2==1 |
                                dat$treatment___3==1 | dat$treatment___4==1 |
                                dat$treatment___5==1 |dat$treatment___6==1 |
                                dat$treatment___7==1)]
NA -> dat$treat_comorbid[which(dat$treatment___9==1)]
dat$treat_comorbid <- as.factor(dat$treat_comorbid)
# dat <- select(dat, -c(treatment___1 : treatment___9))

#recoding disability variables
dat$disability <- 0
1 -> dat$disability[which(dat$disability___1==1 |dat$disability___2==1 |
                            dat$disability___3==1 | dat$disability___4==1 |
                            dat$disability___5==1 | dat$disability___6==1 |
                            dat$disability___7==1 | dat$disability___9==1)]
NA -> dat$disability[which(dat$disability___10==1)]
dat$disability <- as.factor(dat$disability)

# dropping few health status variables not needed
# dat <- dat %>% select(-c(fitness2, fitness_minutes)) %>% select(-c(disability___1 : disability___10))

###Recode the variables 

table(dat$residence)
# 0 -> dat$residence[which(dat$residence<=4)]
# 1 -> dat$residence[which(dat$residence==5)]
NA -> dat$residence[which(dat$residence==6)]

table(dat$education)
0 -> dat$education[which(dat$education==1 | dat$education==2)]
1 -> dat$education[which(dat$education==3 | dat$education==4 |dat$education==5)]
2 -> dat$education[which(dat$education==6)]
NA-> dat$education[which(dat$education==7)]

#creating new variable for employment status
dat$employ <- 0

#Start with student, but if they have employment, then pick those
NA -> dat$employ[which(dat$employ___9==1)]
2 -> dat$employ[which(dat$employ___4==1)]
1 -> dat$employ[which(dat$employ___1==1 |dat$employ___2==1 |dat$employ___3==1)]
0 -> dat$employ[which(dat$employ___5==1 | dat$employ___6==1 | dat$employ___7==1 |dat$employ___8==1)]
dat <- select(dat, -c(employ___1:employ___9))

table(dat$house_income)
# 0 -> dat$house_income[which(dat$house_income<= 3)]
# 1 -> dat$house_income[which(dat$house_income==4 | dat$house_income==5)]
# 2 -> dat$house_income[which(dat$house_income==6)]
NA -> dat$house_income[which(dat$house_income==7 | dat$house_income==8)]

table(dat$ind_income)
# 0 -> dat$ind_income[which(dat$ind_income<=3)]
# 1 -> dat$ind_income[which(dat$ind_income==4 | dat$ind_income==5)]
# 2 -> dat$ind_income[which(dat$ind_income==6)]
NA-> dat$ind_income[which(dat$ind_income==7 | dat$ind_income==8)]

table(dat$rural_city)
# 1-> dat$rural_city[which(dat$rural_city<=3)]
# 0-> dat$rural_city[which(dat$rural_city==4)]

table(dat$where_live)
0-> dat$where_live[which((dat$where_live>=1 & dat$where_live <= 9) | dat$where_live==12 | 
                           (dat$where_live>=15 & dat$where_live<=19))]
1 -> dat$where_live[which(dat$where_live==10 | dat$where_live==13 | dat$where_live==14)]
2 -> dat$where_live[which(dat$where_live==11)]
NA -> dat$where_live[which(dat$where_live==20 | dat$where_live==21)]

dat <- dat %>% mutate(select(.,c(comfrt:advnce,norm_q: relig_q,part_q:prob_q)) %>% replace_with_na_all(condition = ~.x == 6))

dat <- dat %>%
  mutate_at(.vars = vars(mentalill:divorce), ~ recode(., '2' = '0','1' = '1',.default=NA_character_))

dat <- dat %>%
  mutate_at(.vars = vars(slap:forced), ~ recode(., '3' = '0','2' = '1', '1'='1',.default=NA_character_))

###Reverse coding for some variables

#reversing not_sig variable

##Internalized homophobia variables
#reverse coding comfrt, public, seen, public_plc, bars
#Are all of these internalized homophobia? Doesnt feel like it?
#But I suppose it's been validated? 

dat[,c("not_sig","comfrt","public","seen","public_plc","bars")] = 6 -
  dat[,c("not_sig","comfrt","public","seen","public_plc","bars")]

###Explore data

dat$receive_help <-
  case_when(
    dat$help_delay == 1 ~ 0,
    dat$help_delay == 0 ~ 1, 
    dat$help_delay == 2 ~ 0)

dat2 <- dat %>% filter(receive_help %in% c(1,0))

#seek_help and help_delay combined

new<- dat2 %>% select(.,c(cesd_score,cesd1_score,record_id,receive_help)) %>% gather(key=time,value=score, cesd_score:cesd1_score, factor_key=TRUE)

p <- ggplot(data = new, aes(x = time, y = score, group = record_id))
p + geom_line() + facet_grid(. ~ receive_help)

p <- ggplot(data = dat2, aes(x = diff_cesd))
p + geom_histogram() + facet_grid(. ~ receive_help)

t.test(dat$cesd_score,dat$cesd1_score,paired=TRUE)
#No strong difference in the changes of the scores

t.test(diff_cesd ~ receive_help,data=dat,var.equal=TRUE)
#among those who wanted to seek help, got a lot worse for sure, maybe because
#they were more severe

dat3 <- dat %>% subset(help_delay %in% c(0,1)) 

t.test(diff_cesd ~ help_delay,data=subset(dat3,help_delay != 2),var.equal=TRUE)

save(dat,file="savedata.Rdata")
