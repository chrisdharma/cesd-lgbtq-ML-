#load data
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


#Include all predictors
dat<- dat[dat$baseline_complete==2,]

dat_vars <- dat %>% select(.,c(suicidal,age,province,curr_orient2,gender,trans,intersex,
poc,residence,education,house_income,ind_income,rural_city,where_live,
gen_health,fitness1,mental_health,stresslife,diagnosis,cesd1_score,feel1,feel2,feel3,feel4,feel5,feel6,feel7,
con_eating,con_anxiety,con_ADD,con_ADHD,con_depression,treat_comorbid,disability,
con_OCD,con_panic,con_PTSD,con_others,con_bipolar,receive_help,
cigs_smoked,curr_smoke,use_cigar,use_wp,use_smokeless,covid,risk1,risk2,risk3,risk4,
plan_quit,quit_attempts,tailored,quit_support,time_vape,curr_vape,alcohol,disability___1,disability___2,
disability___3,disability___4,disability___5,disability___6,disability___7,disability___9,disability,
alcohol_amount,cannabis,drug_12m,poppers,crystal_meth,crack,cocaine,heroin,pres_opioids,
treatment___1,treatment___2,treatment___3,treatment___4,treatment___5,
fentanyl,GHB,tranquilizers,special_K,MDMA,psychedelics,drug_others,substances_covid,seek_help,central,not_sig,imprtant,
understand,mom,dad,sibs,partner,ext_fam,new_frnd,old_frnd,co_work,employr,relig_mem,stranger,
famdoc,oth_hlth,classmt,teach,part_q,pos_q,bond_q,proud_q,polit_q,solv_q,prob_q,norm_q,
pass_q,relat_q,hit_q,police_q,live_q,job_q,names_q,asslt_q,frnd_q,
hurt_q,fam_q,relig_q,comfrt,control,public,change,seen,sustain,anonym,promisc,
nervous,public_plc,bars,advnce,rates,accept,stress,stigma,pressure,mhealth,
culture,mentalill,drinker,streetdrug,jail,divorce,slap,
beat,swear,inapptouch,inapptouchyou,forced,employ,ethnicity))

summary(dat_vars)
# dat_fin <- na.omit(dat_vars)

dat_vars[sapply(dat_vars, is.nan)] <- NA

lapply(dat_vars,function(i) {
  table(i, useNA="ifany")})

#now no more variables has NaN.

#50% missing is when it's unusable according to Aya and no variables have > 50% missing
##Some variables have NaN instead of NA, let's transform those to NA

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
names<-c("province","curr_orient2","gender","trans","intersex","ethnicity","treat_comorbid","disability",
         "disability___1","disability___2","disability___3","disability___4","disability___5","disability___6","disability___7",
         "poc","residence","education","house_income","ind_income","rural_city",
         "where_live","fitness1","diagnosis","alcohol","alcohol_amount","cannabis","drug_12m","poppers","crystal_meth","crack","cocaine","heroin","pres_opioids",
         "fentanyl","GHB","tranquilizers","special_K","MDMA","psychedelics","drug_others",
         "con_eating","con_anxiety","con_ADD","con_ADHD","con_depression","con_bipolar",
         "treatment___1","treatment___2","treatment___3","treatment___4","treatment___5",
         "con_OCD","con_panic","con_PTSD","con_others","cigs_smoked","curr_smoke","use_cigar","use_wp","use_smokeless","covid",
         "plan_quit","quit_attempts","tailored","quit_support","time_vape",
         "curr_vape","substances_covid","seek_help","employ")
dat_vars[,names]<-lapply(dat_vars[,names],factor)

names2<-c("mentalill","drinker","streetdrug","jail","divorce","slap","beat","swear",
          "inapptouch","inapptouchyou","forced")

dat_vars[,names2]<-sapply(dat_vars[,names2],as.numeric)

##Imputation for missing data. 
dat1.hd <- dat_vars

#retaining unimputed dataset as dat.hd

##seek help and mental health are very important, which makes sense, so we can stratify based on seekhelp
###Need to do the imputation to ensure adequate sample sizes

#From the older codes
#using mice for data imputation
library(mice)
init <- mice(dat1.hd, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

predM[,"cesd1_score"] <- 0

set.seed(123)
imputed.hd <- mice(dat1.hd, method='pmm', predictorMatrix=predM, m=5)
#pmm = imputation for any types of variables, rather than LDA

# select the first copy
df1 <- complete(imputed.hd,1)
df2 <- complete(imputed.hd,2)
df3 <- complete(imputed.hd,3)
df4 <- complete(imputed.hd,4)
df5 <- complete(imputed.hd,5)
#check out the MICE package guide, with complete function, it gives a lot of warning too!

#Create function to scale the data on the data frames

scalevars <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
  df <- df %>% mutate(cen_identity = select(.,central:understand) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(outness = select(.,mom:teach) %>% rowMeans(na.rm=TRUE)) 
  df <- df %>% mutate(connect_com = select(.,part_q: prob_q) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(ace = select(.,mentalill: forced) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(phobia = select(.,comfrt: advnce) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(per_stigma = select(.,c(norm_q:relat_q,hurt_q,fam_q)) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(en_stigma = select(.,c(hit_q: frnd_q,relig_q)) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(cesd_score = select(.,c(feel1: feel7)) %>% rowSums(na.rm=TRUE)) 
  # df <- df %>% mutate(per_sm_risk = select(.,c(risk1: risk4)) %>% rowSums(na.rm=TRUE)) 
  # df <- df %>% mutate(per_sm_lgbtq = select(.,c(accept: culture)) %>% rowSums(na.rm=TRUE)) 
  df[,c("cen_identity", "outness", "connect_com", "ace", "phobia", "per_stigma","en_stigma","cesd_score")] <- 
    df %>% select(cen_identity, outness, connect_com, ace, phobia, per_stigma, en_stigma, cesd_score) %>% scale()
  df <- df %>% select(-c(central:understand,mom:teach,part_q:prob_q,mentalill:forced,
                         comfrt: advnce,norm_q:relat_q,hurt_q,fam_q,hit_q: frnd_q,relig_q, feel1:feel7))
  return(df)
}

# https://towardsdatascience.com/how-data-normalization-affects-your-random-forest-algorithm-fbc6753b4ddf

df1<-scalevars(df1)
df2<-scalevars(df2)
df3<-scalevars(df3)
df4<-scalevars(df4)
df5<-scalevars(df5)

lapply(df1,function(i) {
  table(i, useNA="ifany")})
#now got zero missing after MICE!

#predict rf with eval in a test set
evalrf <- function(df) {

splitIndex <- createDataPartition(df$cesd1_score,p=0.8,times=1,list=FALSE)
train.df <- df[splitIndex,]
test.df <- df[-splitIndex,]

trControl <- trainControl(method = "repeatedcv", number = 10, repeats= 3, search = "grid")
mtrygrid <- expand.grid(.mtry=c(1:25))

rf.df <- train(cesd1_score ~ ., data = train.df,method = "rf",
                trControl=trControl,metric="RMSE")

## get prediction
prediction.df <- predict(rf.df, test.df)
##gave the best RMSE with mtry = 2
#assess prediction

#This gives the prediction value, RMSE on test.df1 using mdoel obtained from rf.df1
pred<-mean((test.df$cesd1_score-predict(rf.df,test.df))^2)
return(list(rf.df,pred))
}

importance <- function(rf) {
  #Show the importance of the variables from each rf
  vip1 <- vip(rf[[1]],scale=TRUE)
  imp <- data.frame(vip1$data)
  return(list(vip1,imp))
}

#Return the rf as an object and performance of the prediction (RMSE on test set)

set.seed(1256)
rf1<-evalrf(df1)
importance(rf1)
rf1[[2]]

set.seed(9013)
rf2<-evalrf(df2)
importance(rf2)
rf2[[2]]

set.seed(5871)
rf3<-evalrf(df3)
importance(rf3)
rf3[[2]]

set.seed(23)
rf4<-evalrf(df4)
importance(rf4)
rf4[[2]]

set.seed(62)
rf5<-evalrf(df5)
importance(rf5)
rf5[[2]]

df<-df1
rf.sh<-rf1[[1]]
importance(rf1)

##############correlation analysis
cor<- (DescTools::PairApply(df, DescTools::CramerV))
#total 3721 pairs
corrplot(cor, method="circle")

#prepare to drop duplicates and correlations of 1     
cor[lower.tri(cor,diag=TRUE)] <- NA 
#drop perfect correlations
cor[cor == 1] <- NA 
cor <- as.data.frame(as.table(cor))
#remove the NA values from above 
cor <- na.omit(cor) 

#subsetting corr more than 0.60
cor <- subset(cor, abs(Freq) >= 0.6)
#A few had high correlations
mtx_cor <- reshape2::acast(cor, Var1~Var2, value.var="Freq")

corrplot(mtx_cor, is.corr=FALSE, method="circle", na.label=" ")

####partial dependence of top 10 correlates
##See from imp1

#So everything is continuous except mental_health
#Read up on what PDP are, partial dependence 
pdp_continuous <- function(variable,xvar,label) {
  pdp.partial <- partial(rf.sh, xvar)
  pdp.plot <- plotPartial(pdp.partial,  xlab= label, ylab= 'PD')
  return(print(pdp.plot))
}
# https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf

pdp1<-pdp_continuous(variable=cesd_score,xvar='cesd_score', label = 'baseline depressive symptoms')
pdp2<-pdp_continuous(variable=per_stigma,xvar='per_stigma', label = 'perceived stigma')
pdp3<-pdp_continuous(variable=suicidal,xvar='suicidal', label = 'suicidality')
pdp4<-pdp_continuous(variable=connect_com,xvar='connect_com',label = 'community connectedness')
pdp5<-pdp_continuous(variable=ace,xvar='ace', label = 'adverse childhood experience')
pdp6<-pdp_continuous(variable=en_stigma,xvar='en_stigma', label = 'enacted stigma')
pdp7<-pdp_continuous(variable=outness,xvar='outness', label = 'overall outness')
pdp8<-pdp_continuous(variable=mental_health,xvar='mental_health', label = 'self-rated mental health')
pdp9<-pdp_continuous(variable=phobia,xvar='phobia',label = 'internalized homophobia')
pdp10<-pdp_continuous(variable=age,xvar='age',label = 'age')

# #binary only one variable, so we dont need to create another function
# pdp.mentalhealth <- partial(rf1[[1]], "mental_health", which.class= 2)
# pdp.mentalhealth$mental_health <- factor(pdp.mentalhealth$mental_health, levels = c(0,1), labels= c('Poor or fair', 'Good or excellent'))
# pdp.mentalhealth.p <- plotPartial(pdp.mentalhealth,  xlab= 'Mental health', ylab= 'PD')

library(ggpubr)

ggarrange(pdp1,pdp2,pdp3,pdp4,pdp5,pdp6,pdp7,pdp8,pdp9,pdp10,
          ncol = 5, nrow = 2)

# str(df1[,c("province","curr_orient2","curr_gender","trans","intersex","ethnicity",
#          "poc","residence","education","house_income","ind_income","rural_city",
#          "where_live","fitness1","gen_health","mental_health","stresslife","diagnosis",
#          "con_eating","con_anxiety","con_ADD","con_ADHD","con_depression","con_bipolar",
#           "con_OCD","con_panic","con_PTSD","con_others","cigs_smoked","curr_smoke","use_cigar","use_wp","use_smokeless","covid",
#          "plan_quit","quit_attempts","tailored","quit_support","time_vape",
#          "curr_vape","alcohol","alcohol_amount","cannabis","drug_12m",
#          "substances_covid","seek_help","employ")])
#panel of top 10 correlates
# pdp.sh.categorical<- ggarrange(pdp.mentalhealth.p,pdp.suicidal.p,
#                                labels= c('A', 'B'),
#                                nrow=1)
# print(pdp.sh.categorical)
# 
# pdp.sh.continuous <- ggarrange(pdp.outness.p, pdp.en_stigma.p,
#                                pdp.phobia.p,  pdp.per_stigma.p,
#                                pdp.connect_com.p, pdp.cen_identity.p, 
#                                pdp.age.p, pdp.ace.p,
#                                labels= c('C', 'D', 'E', 'F', 'G', 'H','I', 'J'),
#                                nrow=2,ncol=4)
# print(pdp.sh.continuous)
# 
# pdp.sh <- ggarrange(pdp.sh.categorical, pdp.sh.continuous,
#                     ncol=1, heights= c(0.5, 1))
# print(pdp.sh)

################interactions
#interaction strength between demographics

# all_pairs<-combn(paste0(colnames(subset(df,select=-c(diff_cesd)))),m=2)
# 
# res <- NULL
# for (i in seq_along(all_pairs)) {
#   interact <- vint(rf.sh, feature_names = all_pairs[, i], n.trees = best_iter)
#   res <- rbind(res, interact)
# }

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

#To interpret strong interactions, maybe can make it binary
#curr_orient vs top 10 predictors

interaction_top10 <- function(dem) {
int1<-vint(rf.sh, feature_names = c(dem, 'connect_com'))
int2<-vint(rf.sh, feature_names = c(dem, 'outness'))
int3<-vint(rf.sh, feature_names = c(dem, 'mental_health'))
int4<-vint(rf.sh, feature_names = c(dem, 'per_stigma'))
int5<-vint(rf.sh, feature_names = c(dem, 'ace'))
int6<-vint(rf.sh, feature_names = c(dem, 'phobia'))
int7<-vint(rf.sh, feature_names = c(dem, 'en_stigma'))
int8<-vint(rf.sh, feature_names = c(dem, 'cesd_score'))
int9<-vint(rf.sh, feature_names = c(dem, 'suicidal'))
c1<-c(int1[1,2],int2[1,2],int3[1,2],int4[1,2],int5[1,2],int6[1,2],
      int7[1,2],int8[1,2],int9[1,2])
c2<-c("connect_com","outness","mental_health","per_stigma","ace","phobia","en_stigma","cesd_score","suicidal")
c3<-c(dem)
tab<-cbind(c1,c2,c3)
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
write.xlsx(intall4, file="intall3_OCT6.xlsx", sheetName="sheet1a")

##Try the lasso

###Some were comparing NAs
library(glmnet)

lasso <- function(df) {
splitIndex <- createDataPartition(df$cesd1_score,p=0.8,times=1,list=FALSE)
x <- model.matrix(cesd1_score ~ .,df)[,-1]
y.train <- df[splitIndex,]$cesd1_score
y.test <- df[-splitIndex,]$cesd1_score

#Create the model
lr.mod <- glmnet(x[splitIndex,],y.train,family="gaussian",alpha=1)
plot(lr.mod)
plot(lr.mod,xvar = "lambda")

#Find the best lambda through CV
cv.lr <- cv.glmnet(x[splitIndex,],y.train,alpha=1)
plot(cv.lr)

#lambda.1se will result in simpler model compared to lambda.min

#Check prediction

rmse1<-mean((y.test-predict(lr.mod, newx=x[-splitIndex,],s= c(cv.lr$lambda.min)))^2)
coef.min1 <- coef(cv.lr, s ="lambda.min")
i<-coef.min1@i #indexes of selected coefficients
return(list(rmse1,coef.min1,colnames(x)[i]))
}

set.seed(156)
lasso1<-lasso(df1)

set.seed(826)
lasso2<-lasso(df2)

set.seed(103)
lasso3<-lasso(df3)

set.seed(523)
lasso4<-lasso(df4)

set.seed(4093)
lasso5<-lasso(df5)

lasso1[[3]]
lasso2[[3]]
lasso3[[3]]
lasso4[[3]]
lasso5[[3]]

lasso1[[1]]
lasso2[[1]]
lasso3[[1]]
lasso4[[1]]
lasso5[[1]]

demovars<-c('age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')
demovars2<-c('province','age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')

factorVars<-c('province','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')

tab<-CreateTableOne(vars=demovars2, factorVars = factorVars,data=dat_vars)
print(summary(tab),quote=TRUE,noSpaces=TRUE)
tab2<-as.data.frame(print(tab))
print(tab,quote=TRUE,noSpaces=TRUE)

library(xlsx)
write.xlsx(tab2, file="dem.xlsx", sheetName="sheet1a")

#Consider Interactions with LASSO

splitIndex <- createDataPartition(df$cesd1_score,p=0.8,times=1,list=FALSE)
# .*. include all ways interactions
# .^2 include all pairwise interactions

x <- model.matrix(cesd1_score ~ .^2,df)[,-1]
y.train <- df[splitIndex,]$cesd1_score
y.test <- df[-splitIndex,]$cesd1_score

#Create the model
lr.mod <- glmnet(x[splitIndex,],y.train,family="gaussian",alpha=1)
plot(lr.mod)
plot(lr.mod,xvar = "lambda")

#Find the best lambda through CV
cv.lr <- cv.glmnet(x[splitIndex,],y.train,alpha=1)
plot(cv.lr)

#lambda.1se will result in simpler model compared to lambda.min

#Check prediction

rmse1<-mean((y.test-predict(lr.mod, newx=x[-splitIndex,],s= c(cv.lr$lambda.1se)))^2)
coef.min1 <- coef(cv.lr, s ="lambda.1se")
i<-coef.min1@i #indexes of selected coefficients
colnames(x)[i]
#If we use lambda.1se, gives much much fewer interactions, and it's all 2 way interactions, not identifying them with RF

