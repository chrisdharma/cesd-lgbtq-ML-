#Program 2 to predict new mental health diagnosis among those who were never diagnosed with mental health problems
#subsetting was done later, imputation was done using everyone's data to maximize power

#load data
#Load packages
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
library(stats)
library(epiR)
library(pROC)


##PART 2 ANALYSIS - new diagnosis of mental health problems

#Include all predictors

#disability
#treat_comorbid
# drug_12m
#mental health conditions cannot be used, since nobody has mental health conditions previously, so these predictors were not added, unlike program 1

#Combine two spirit and questioning, as these are too small (made as other)
dat <- dat %>% mutate(curr_orient3 = ifelse(curr_orient2 %in% c(7,8),7,curr_orient2)) %>%
        mutate(curr_orient2 = curr_orient3) %>%
        select(-c(curr_orient3))

dat_vars <- dat %>% select(.,c(suicidal,age,province,curr_orient2,gender,trans,intersex,
                               poc,residence,education,house_income,ind_income,rural_city,where_live,
                               gen_health,fitness1,mental_health,stresslife,diagnosis,treat_comorbid,disability,
                               receive_help,cigs_smoked,curr_smoke,use_cigar,use_wp,use_smokeless,covid,risk1,risk2,risk3,risk4,
                               plan_quit,quit_attempts,tailored,quit_support,time_vape,curr_vape,alcohol,disability,
                               alcohol_amount,cannabis,drug_12m,substances_covid,seek_help,central,not_sig,imprtant,
                               understand,mom,dad,sibs,partner,ext_fam,new_frnd,old_frnd,co_work,employr,relig_mem,stranger,
                               famdoc,oth_hlth,classmt,teach,part_q,pos_q,bond_q,proud_q,polit_q,solv_q,prob_q,norm_q,
                               pass_q,relat_q,hit_q,police_q,live_q,job_q,names_q,asslt_q,frnd_q,
                               hurt_q,fam_q,relig_q,comfrt,control,public,change,seen,sustain,anonym,promisc,
                               nervous,public_plc,bars,advnce,rates,accept,stress,stigma,pressure,mhealth,
                               culture,mentalill,drinker,streetdrug,jail,divorce,slap,cesd_score,
                               beat,swear,inapptouch,inapptouchyou,forced,employ,ethnicity,diagnosis_1))

dat_fin <- na.omit(dat_vars)

dat_vars[sapply(dat_vars, is.nan)] <- NA

dat_fin_analysis<-subset(dat_vars,diagnosis != 1)

lapply(dat_vars,function(i) {
  table(i, useNA="ifany")})

##missing data
sapply(dat_vars, function(x) sum(is.na(x)))

vis_miss(dat_vars,sort_miss=TRUE) 
#2.4% missing data

q.hd <- dat_vars %>% summarise_all(~sum(is.na(.)))
q2.hd <- t(q.hd)
q3.hd <- data.frame('Number_of_missingness'= q2.hd[,1],
                    'percent_missingness'=round(q2.hd[,1]/nrow(dat_vars)*100, digit=2))
# sort
q3.hd <- q3.hd[order(q3.hd$percent_missingness,q3.hd$Number_of_missingness),]

# how many vars had >=5% missing?
m.hd <- q3.hd[q3.hd$percent_missingness>=5,]
dim(m.hd)
#17 variables have >= 5% missing data
#Some were above 10%

#change factors 
names<-c("province","curr_orient2","gender","trans","intersex","ethnicity","disability",
         "poc","residence","education","house_income","ind_income","rural_city",
         "where_live","fitness1","diagnosis","alcohol","alcohol_amount","cannabis","drug_12m",
         "treat_comorbid","cigs_smoked","curr_smoke","use_cigar","use_wp","use_smokeless","covid",
         "plan_quit","quit_attempts","tailored","quit_support","time_vape",
         "curr_vape","substances_covid","seek_help","employ","diagnosis_1")

dat_vars[,names]<-lapply(dat_vars[,names],factor)

names2<-c("mentalill","drinker","streetdrug","jail","divorce","slap","beat","swear",
          "inapptouch","inapptouchyou","forced")
dat_vars[,names2]<-sapply(dat_vars[,names2],as.numeric)

#Create ID
dat_vars$ID <- seq(1:nrow(dat_vars))

##Imputation for missing data
dat1.hd <- dat_vars
#retaining unimputed dataset as dat1.hd

table(dat1.hd$diagnosis)
ftable(dat1.hd$diagnosis,dat1.hd$diagnosis_1)

#We want to use everyone's information to impute, but later only include those who weren't diagnosed
dat1.hd <- dat1.hd %>% select(-c(diagnosis))

library(mice)
init <- mice(dat1.hd, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

# exclude help-delay as predictor
predM[,"diagnosis_1"] <- 0

set.seed(123)
imputed.hd <- mice(dat1.hd, method='pmm', predictorMatrix=predM, m=5)
#pmm = imputation for any types of variables, rather than LDA
summary(imputed.hd)

# dat1.hd<-subset(dat1.hd,diagnosis != 1)
# select the first copy
df1 <- complete(imputed.hd,1)
df2 <- complete(imputed.hd,2)
df3 <- complete(imputed.hd,3)
df4 <- complete(imputed.hd,4)
df5 <- complete(imputed.hd,5)

#Create function to impute data on the data frames
scalevars <- function(df) {
  df <- df %>% mutate(cen_identity = select(.,central:understand) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(outness = select(.,mom:teach) %>% rowMeans(na.rm=TRUE)) 
  df <- df %>% mutate(connect_com = select(.,part_q: prob_q) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(ace = select(.,mentalill: forced) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(phobia = select(.,comfrt: advnce) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(per_stigma = select(.,c(norm_q:relat_q,hurt_q,fam_q)) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(en_stigma = select(.,c(hit_q: frnd_q,relig_q)) %>% rowSums(na.rm=TRUE)) 
  # df <- df %>% mutate(per_sm_risk = select(.,c(risk1: risk4)) %>% rowSums(na.rm=TRUE)) 
  # df <- df %>% mutate(per_sm_lgbtq = select(.,c(accept: culture)) %>% rowSums(na.rm=TRUE)) 
  df[,c("cen_identity", "outness", "connect_com", "ace", "phobia", "per_stigma","en_stigma","cesd_score")] <- 
  df %>% select(cen_identity, outness, connect_com, ace, phobia, per_stigma, en_stigma,cesd_score) %>% scale()
  df <- df %>% select(-c(central:understand,mom:teach,part_q:prob_q,mentalill:slap,beat:forced,
                         comfrt: advnce,norm_q:relat_q,hurt_q,fam_q,hit_q: frnd_q,relig_q))
  df$ID<-seq(1:nrow(df))
  df2 <- merge(df,dat_vars[,c("diagnosis","ID")],by="ID")
  return(df2)
}

# https://towardsdatascience.com/how-data-normalization-affects-your-random-forest-algorithm-fbc6753b4ddf

df1<-scalevars(df1)
df2<-scalevars(df2)
df3<-scalevars(df3)
df4<-scalevars(df4)
df5<-scalevars(df5)

subsetdiag <- function(df) {
  df<-subset(df,diagnosis != 1)
  df<-df %>% select(-c(diagnosis,ID))
  return(df)
}

df1<-subsetdiag(df1)
df2<-subsetdiag(df2)
df3<-subsetdiag(df3)
df4<-subsetdiag(df4)
df5<-subsetdiag(df5)

#predict rf with eval in a test set
#Just to check the frequencies

lapply(df1,function(i) {
  table(i, useNA="ifany")})

evalrf <- function(df) {
  splitIndex <- createDataPartition(df$diagnosis_1,p=0.8,times=1,list=FALSE)
  train.df <- df[splitIndex,]
  test.df <- df[-splitIndex,]
  
  trControl <- trainControl(method = "repeatedcv", number = 10, repeats= 3, search = "grid")
  mtrygrid <- expand.grid(.mtry=c(1:25))
  
  rf.df <- train(diagnosis_1 ~ ., data = train.df,method = "rf",
                 trControl=trControl,metric="Accuracy")
  
  prediction.df <- predict(rf.df, test.df,type="raw")
  tb.dc <- table(as.numeric(as.character(prediction.df)), as.numeric(as.character(test.df$diagnosis_1)))
  res1<-confusionMatrix(tb.dc)
  res2<-epi.tests(tb.dc)
  #AUC
  pROC_obj <- roc(response=test.df$diagnosis_1,predict=as.numeric(as.character(prediction.df)),
                  smoothed = TRUE,ci=TRUE, ci.alpha=0.95, stratified=FALSE)  
  #Brier scores
  res3 <- colMeans((as.numeric(as.character(test.df$diagnosis_1)) - predict(rf.df, test.df,type="prob")["1"])^2)
  return(list(rf.df,res1,res2,res3,pROC_obj,prediction.df))
}

importance <- function(rf) {
  #Show the importance of the variables from each rf
  vip1 <- vip(rf[[1]],scale=TRUE)
  imp <- data.frame(vip1$data)
  return(list(vip1,imp))
}

set.seed(3580)
rf1<-evalrf(df1)
rf1[[4]]
rf1[[5]]

set.seed(3496)
rf2<-evalrf(df2)
rf2[[4]]
rf2[[5]]

set.seed(5690)
rf3<-evalrf(df3)
rf3[[4]]
rf3[[5]]

set.seed(209)
rf4<-evalrf(df4)
rf4[[4]]
rf4[[5]]

set.seed(5680)
rf5<-evalrf(df5)
rf5[[4]]
rf5[[5]]

importance(rf1)
importance(rf2)
importance(rf3)
importance(rf4)
importance(rf5)

#Best performance is RF2, df2 with the lowest Brier score, so we will continue using this one

df<-df4
rf.sh<-rf4[[1]]

##############correlation analysis
cor<- (DescTools::PairApply(df, DescTools::CramerV))
corrplot(cor, method="circle")

#prepare to drop duplicates and correlations of 1     
cor[lower.tri(cor,diag=TRUE)] <- NA 
#drop perfect correlations
cor[cor == 1] <- NA 
cor <- as.data.frame(as.table(cor))
#remove the NA values from above 
cor <- na.omit(cor) 

#subsetting corr more than 0.60 (too small, so i made threshold bigger)
cor <- subset(cor, abs(Freq) >= 0.6)
#A few had high correlations
mtx_cor <- reshape2::acast(cor, Var1~Var2, value.var="Freq")

corrplot(mtx_cor, is.corr=FALSE, method="circle", na.label=" ")

####partial dependence of top 10 correlates
##See from imp1

pdp_continuous <- function(variable,xvar,label) {
  pdp.partial <- partial(rf.sh, xvar,which.class=2)
  pdp.plot <- plotPartial(pdp.partial,  xlab= label, ylab= 'PD')
  return(print(pdp.plot))
}

pdp1<-pdp_continuous(variable=per_stigma,xvar='per_stigma', label = 'perceived stigma')
pdp2<-pdp_continuous(variable=stresslife,xvar='stresslife', label = 'stressful life')
pdp3<-pdp_continuous(variable=phobia,xvar='phobia',label = 'internalized homophobia')
pdp4<-pdp_continuous(variable=ace,xvar='ace',label = 'adverse childhood experience')
pdp5<-pdp_continuous(variable=cesd_score,xvar='cesd_score', label = 'depressive symptoms score')
pdp6<-pdp_continuous(variable=age,xvar='age',label = 'age')
pdp7<-pdp_continuous(variable=mental_health,xvar='mental_health',label = 'self-rated mental health')
pdp8<-pdp_continuous(variable=outness,xvar='outness', label = 'overall outness')
pdp9<-pdp_continuous(variable=connect_com,xvar='connect_com',label = 'community connectedness')
pdp10<-pdp_continuous(variable=en_stigma,xvar='en_stigma',label = 'enacted stigma')

library(ggpubr)

ggarrange(pdp1,pdp2,pdp3,pdp4,pdp5,pdp6,pdp7,pdp8,pdp9,pdp10,
          ncol = 5, nrow = 2)

demovars<-c('age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live')

interact.sh <- vint(rf.sh, feature_names = demovars)
interact.sh$Interaction 

interactiondem<-as.data.frame(interact.sh)

# plot the interaction strength
int.sh_plot <- ggplot(data=interact.sh,aes(reorder(Variables, Interaction), Interaction))+
  geom_bar(stat="identity",fill=NA,color="black")+
  geom_text(aes(label=Interaction),color="black",position=position_dodge(0.1),hjust=-0.1)+
  coord_flip()+
  labs(y="Interaction strength",
       x="Pairwise interaction")+
  theme_classic()
print(int.sh_plot)

interaction_top10 <- function(dem) {
  int1<-vint(rf.sh, feature_names = c(dem, 'stresslife'))
  int2<-vint(rf.sh, feature_names = c(dem, 'mental_health'))
  int3<-vint(rf.sh, feature_names = c(dem, 'per_stigma'))
  int4<-vint(rf.sh, feature_names = c(dem, 'cesd_score'))
  int5<-vint(rf.sh, feature_names = c(dem, 'ace'))
  int6<-vint(rf.sh, feature_names = c(dem, 'phobia'))
  int7<-vint(rf.sh, feature_names = c(dem, 'outness'))
  int8<-vint(rf.sh, feature_names = c(dem, 'connect_com'))
  int9<-vint(rf.sh, feature_names = c(dem, 'en_stigma'))
  c1<-c(int1[1,2],int2[1,2],int3[1,2],int4[1,2],int5[1,2],int6[1,2],
        int7[1,2],int8[1,2],int9[1,2])
  c2<-c("stresslife","mental_health","per_stigma","cesd_score","ace","phobia","outness","connect_com","en_stigma")
  c3<-c(dem)
  tab<-cbind(c1,c2,c3)
  if (dem != "age") {
    int10<-vint(rf.sh, feature_names = c(dem, 'age'))
    c1<-c(c1,int10[1,2])
    c2<-c(c2,"age")
    tab<-cbind(c1,c2,c3)
  }
  return(tab)
}

int1<-interaction_top10(dem="curr_orient2")
int2<-interaction_top10(dem="gender")
int3<-interaction_top10(dem="ethnicity")
int4<-interaction_top10(dem="education")
int5<-interaction_top10(dem="employ")
int6<-interaction_top10(dem="house_income")
int7<-interaction_top10(dem="where_live")
int8<-interaction_top10(dem="age")

intall<-as.data.frame(rbind(int1,int2,int3,int4,int5,int6,int7,int8))
rownames(intall)<-NULL

intall2<-intall %>% rename(Interaction = c1,Variables1 = c2,Variables2 = c3) %>%
  mutate(Variables=paste(Variables1,Variables2,sep="*")) %>%
  select(-c(Variables1,Variables2))
intall3<-rbind(intall2,interactiondem)

intall3$Interaction <- as.numeric(as.character(intall3$Interaction))
intall4 <- intall3 %>% arrange(desc(Interaction))
intall4

library(xlsx)
write.xlsx(intall4, file="intall2_OCT6.xlsx", sheetName="sheet1a")

###Do the LASSO

library(glmnet)

lasso <- function(df) {
  splitIndex <- createDataPartition(df$diagnosis_1,p=0.8,times=1,list=FALSE)
  x <- model.matrix(diagnosis_1 ~ .,df)[,-1]
  y.train <- df[splitIndex,]$diagnosis_1
  y.test <- df[-splitIndex,]$diagnosis_1
  
  #Create the model
  lr.mod <- glmnet(x[splitIndex,],y.train,family="binomial",alpha=1)
  plot(lr.mod)
  plot(lr.mod,xvar = "lambda")
  
  #Find the best lambda through CV
  cv.lr <- cv.glmnet(x[splitIndex,],y.train,family = "binomial", type.measure = "mse",alpha=1)
  plot(cv.lr)
  
  #lambda.1se will result in simpler model compared to lambda.min
  
  #Check prediction
  predict.df <- predict(lr.mod, newx=x[-splitIndex,],type="response",s= c(cv.lr$lambda.min))
  brier <- mean((as.numeric(as.character(y.test)) - predict.df)^2)
  pROC_obj <- roc(response=y.test,predict=as.numeric(as.character(predict.df)),
                  smoothed = TRUE,ci=TRUE, ci.alpha=0.95, stratified=FALSE)  
  
  coef.min1 <- coef(cv.lr, s ="lambda.min")
  i<-coef.min1@i #indexes of selected coefficients
  return(list(brier,coef.min1,colnames(x)[i],pROC_obj))
}

set.seed(908)
lasso1<-lasso(df1)

set.seed(756)
lasso2<-lasso(df2)

set.seed(340)
lasso3<-lasso(df3)

set.seed(100)
lasso4<-lasso(df4)

set.seed(2099)
lasso5<-lasso(df5)

lasso1[[1]]
lasso1[[4]]

lasso2[[1]]
lasso2[[4]]

lasso3[[1]]
lasso3[[4]]

lasso4[[1]]
lasso4[[4]]

lasso5[[1]]
lasso5[[4]]
#Best performance with LASSO was df2

lasso1[[3]]
lasso2[[3]]
lasso3[[3]]
lasso4[[3]]
lasso5[[3]]

demovars<-c('age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')
demovars2<-c('province','age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')

factorVars<-c('province','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')

tab<-CreateTableOne(vars=demovars2, factorVars = factorVars,data=dat_fin_analysis)
print(summary(tab),quote=TRUE,noSpaces=TRUE)
tab2<-as.data.frame(print(tab))
print(tab,quote=TRUE,noSpaces=TRUE)

library(xlsx)
write.xlsx(tab2, file="dem344.xlsx", sheetName="sheet1a")


