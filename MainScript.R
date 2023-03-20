#13/07

################################################################################
#-------------------------------------------------------------------------------
################################################################################
#-------------------------------------------------------------------------------
#HEART FAILURES and RE-HOSPITALIZATIONS
#-------------------------------------------------------------------------------
################################################################################
#-------------------------------------------------------------------------------
################################################################################

library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(leaps)
library(MASS)
library(car)
library(rgl)
library(glmnet)
library( rms )
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(ROCR)
library(GGally)
library(largesamplehl)
library(glme)
library(multilevelTools)
library(extraoperators)
library(JWileymisc)
library(lmerTest)
library(performance)

################################################################################
#-------------------------------------------------------------------------------
#Patients fragility analysis
#-------------------------------------------------------------------------------
################################################################################

#################
#Fragility grade
#################

#First we did a medical research, analyzing the different diagnosis a doctor 
#could make for a patient and then we assigned a level of risk (1-5) to each 
#diagnosis.
#Then we computed a grade (1-6) to describe the situation of each patient, taking 
#into account the diagnosis.

#grade = max(diagnosis_risk_level) + 1*{Sum(diagnosis_levels) >= 10}

#Thanks to this formula we can generalize the specific medical situation of 
#each patient.

#After having assigned a grade to all the patient we wanted to verify the 
#significance of this classification via Manova.

################
#MANOVA One Way
################

#The dataset "PATIENTS3anni" contains all the information about the patients
#over a period of three years since their first hospitalization. (The dead/alive
#classification is about the three years.)

#We performed a Manova test to verify that the patients grouped by the grade
#are significantly different, considering total days passed in hospital and
#life/death rate of each group.

#We put together groups (1-3)
#The groups are (1-3),4,5,6

rm(list=ls())
graphics.off()
load("C:/Users/franc/Desktop/poli/4 anno/applied stat/progetto/logistic morte/PATIENTS3anni.Rda")
attach(PATIENTS3anni)
grado <- factor(grado_ccs)
i1 <- which(grado_ccs%in%c(1:3))
i2 <- which(grado_ccs%in%c(4))
i3 <- which(grado_ccs%in%c(5))
i4 <- which(grado_ccs%in%c(6))
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n4 <- length(i4)
n  <- n1+n2+n3+n4
g  <- 4
p  <- 2

### Verify the assumptions:
# 1)  normality (multivariate) in each group

load("C:/Users/franc/Desktop/poli/4 anno/applied stat/_exam/mcshapiro.test.RData")
Ps <- NULL
for(i in 1:g)
  Ps <- c(Ps, mcshapiro.test(iris[get(paste('i',i, sep='')),1:4])$p)   #SISTEMARE
Ps

df = PATIENTS3anni[,c(4,6,7)]
j = which(df$labelOUT=="DECEDUTO")
df$labelOUT = 1
df$labelOUT[j]=0
mcshapiro.test(df[i1,-c(3)])
mcshapiro.test(df[i2,-c(3)])
mcshapiro.test(df[i3,-c(3)])
mcshapiro.test(df[i4,-c(3)])

#Normality not verified

cov(df[i1,-c(3)])
cov(df[i2,-c(3)])
cov(df[i3,-c(3)])
cov(df[i4,-c(3)])


### One-way MANOVA 
df1 = df[,-3]
fit <- manova(as.matrix(df1) ~ grado)
summary.manova(fit,test="Wilks")

summary.aov(fit)

### Via Bonferroni
alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(df1,mean)         
m1 <- sapply(df1[i1,],mean)    
m2 <- sapply(df1[i2,],mean)    
m3 <- sapply(df1[i3,],mean)   
m4 = sapply(df1[i4,],mean)

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
inf14 <- m1-m4 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n4) )
sup14 <- m1-m4 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n4) )
inf43 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n4+1/n3) )
sup43 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n4+1/n3) )
inf24 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n4) )
sup24 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n4) )

CI <- list(uno_due=cbind(inf12, sup12), uno_tre=cbind(inf13, sup13), due_tre=cbind(inf23, sup23),uno_quattro=cbind(inf14, sup14),due_quattro=cbind(inf24, sup24),quattro_tre=cbind(inf43, sup43))
CI
detach(PATIENTS3anni)

#We don't have the normality hypotesis verified, but he means of all the groups 
#look very different so we added the grade as a feature to describe the patients.  
#_______________________________________________________________________________




################################################################################
#-------------------------------------------------------------------------------
#Survival Logistic Regression
#-------------------------------------------------------------------------------
################################################################################

#############
#Motivations
#############

#Since now we have different features to describe the fragility and the risk
#situation of the patients, we moved our attention on the hospitals treatments.
#We selected the hospital which had an amount of patients greater than a certain
#threshold and starting from the patients' three years overall situation, we
#considered the patients who visited always the same hospital. In this way
#we can compare the hospital, taking into account the type of patients they have.

rm(list=ls())
graphics.off()
load("C:/Users/franc/Desktop/poli/4 anno/applied stat/progetto/logistic morte/PATIENTS3anni.Rda")

#Preparing the dataset
table(PATIENTS3anni$labelOUT)
PATIENTS3anni$ngialle=0
PATIENTS3anni$ngialle=rowSums(PATIENTS3anni[,c(42:45)])
ind_tron=which(PATIENTS3anni$labelOUT=='TRONCATO')
quanti = 2500
set.seed(453)
ran=sample.int(length(ind_tron), quanti)
j=ind_tron[ran]
df=PATIENTS3anni[-j,]
table(df$labelOUT)
df$labelOUT=factor(df$labelOUT)
df$grado_ccs=as.numeric(df$grado_ccs)
df_test=df[c(1:500),]
df1=df[-c(1:500),]

#######################
#Logistic Linear Model
#######################

#We started with a logistic regression about the survival probability
#We created a mixed effects linear model, considering the hospital effect
#as a random intercept.

#Response: 

#labelOUT         #1 survive, 0 death


#Regressors: 

#log(eta_media)
#log(num_osp)     #number of hospitalization per patient during the three years
#grado_ccs        #grade of the patient fragility, based on the diagnosis     
#ngialle          #number of interventions (bypass, shock, defibrillator, angioplastic) 
#(1|strutt_id)    #hospital random intercept

l6=glmer(labelOUT ~ log(eta_media)+log(num_osp)+grado_ccs+ngialle+(1|strutt_id),family = binomial, data=df1)
mod=l6


##################################
#Evaluating goodness of the model
##################################

#We prefer specificity over sensibility: it's more important to know
#if a patient could die, to give more attention to him.

soglia=0.7
fit=fitted.values(mod)
fit_bin=rep(0,dim(df1)[1])
fit_bin[which(fit>soglia)]=1
ver=df1$labelOUT
ver_bin=rep(0,dim(df1)[1])
ver_bin[which(ver=='TRONCATO')]=1
tab=table(fit_bin,ver_bin)
tab

soglia=0.7
fut=predict(mod,df_test,allow.new.levels = T,type="response")
fut_bin=rep(0,length(fut))
fut_bin[which(fut>soglia)]=1
ver=df_test$labelOUT
ver_bin=rep(0,dim(df_test)[1])
ver_bin[which(ver=='TRONCATO')]=1
tab=table(fut_bin,ver_bin)
tab
sensibilita =  tab [ 2,2 ] /( tab [ 1,2 ] + tab [2,2] ) 
sensibilita
specificita = tab[ 1,1 ] /( tab [ 1,1 ] + tab [ 2,1 ] )
specificita

x11()
pred <- prediction(fut, ver)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

##################################
#Analysis of the hospital effect
##################################

#We focus on the random intercept given by the hospitals to see if there
#are significant effects on the survival probability after the treatments
#during the hospitalizations.

x11()
dotplot(ranef(mod))
sigma2_eps <- as.numeric(get_variance_residual(mod))
sigma2_b <- as.numeric(get_variance_random(mod))
VPC <- sigma2_b/(sigma2_b+pi*pi/3)
VPC

#6% -> hospitals have different effects on the survival probability

#We can't give a judgment on the quality of an hospital if we don't consider
#the different type of patients it could treats. 
#(for example old fragile patients vs young strong patients)

#So we group the patients by the grade of risk (based on the diagnosis we computed
#at the beginning of the project).

mod=l6
dat=df1
fit=fitted.values(mod)
fit_osp=data.frame(matrix(0,nrow=length(fit),ncol=0))
fit_osp$perc_tronc=as.numeric(round(fit,3))
fit_osp$osp=dat$strutt_id
fit_osp$intercept=0
for(i in c(1:dim(fit_osp)[1])){
  inter=ranef(mod)$strutt_id[fit_osp[i,2],1]
  fit_osp[i,'intercept']=inter
}
m=mean(ranef(mod)$strutt_id[,1])
sd=getME(mod,'theta')

ran=data.frame(cbind(rownames(ranef(mod)$strutt_id),round(ranef(mod)$strutt_id[,1],3)))
colnames(ran)=c('osp','intercept')
ran$posiz=0
for(i in c(1:dim(ran)[1])){
  val=ran[i,2]
  if(val>m+sd) 
    ran[i,3]=1
  else if(val<m-sd)
    ran[i,3]=-1
}

dat$intercept=fit_osp$intercept
dat$perc_tron=fit_osp$perc_tronc
dat$posiz=0
for(i in c(1:dim(ran)[1])){
  osp=ran[i,1]
  j=which(dat$strutt_id==osp)
  dat[j,'posiz']=ran[i,3]
}
#raggruppare 123
effetti=data.frame(matrix(0,ncol=4,nrow=4))
colnames(effetti)=c('grado','_sd1','mean','sd1')
temp=subset(dat,grado_ccs%in%c(1,2,3))
temp1=subset(temp,posiz==-1)
res=round(mean(temp1$perc_tron),2)
effetti[1,'grado']=123
effetti[1,'_sd1']=res
temp1=subset(temp,posiz==0)
res=round(mean(temp1$perc_tron),2)
effetti[1,'mean']=res
temp1=subset(temp,posiz==1)
res=round(mean(temp1$perc_tron),2)
effetti[1,'sd1']=res
g=c(4,5,6)
for(i in g){
  temp=subset(dat,grado_ccs==i)
  temp1=subset(temp,posiz==-1)
  res=round(mean(temp1$perc_tron),2)
  effetti[i-2,'grado']=i
  effetti[i-2,'_sd1']=res
  temp1=subset(temp,posiz==0)
  res=round(mean(temp1$perc_tron),2)
  effetti[i-2,'mean']=res
  temp1=subset(temp,posiz==1)
  res=round(mean(temp1$perc_tron),2)
  effetti[i-2,'sd1']=res
}
effetti
View(effetti)

#We can say that there is evidence on the effects of the hospital, even grouping
#by the fragility grade.
#All the hospital behave well with low risk patients (1-2-3)
#With the situation of the patients getting worse there are hospitals which
#give a better chance of surviving. (Better preparation of the doctors, 
#cleaner room, less overcrowding...(?))
#_______________________________________________________________________________




################################################################################
#-------------------------------------------------------------------------------
#Re-hospitalization Logistic Regression
#-------------------------------------------------------------------------------
################################################################################

rm(list=ls())
graphics.off()
load("C:/Users/franc/Desktop/poli/4 anno/applied stat/progetto/ospedalizz/f2.RData")
load("C:/Users/franc/Desktop/poli/4 anno/applied stat/progetto/ggosp/ospedalizzazioni_ccs_time_cumulative.Rda")

#################################
#Single hospitalization Analysis
#################################

#After considering the situation of the patients over three years, we focus
#on the single hospitalization. In particular we created a logistic regression
#model to predict if a patient will be re-hospitalized within 500 days

#Creating a dataset, considering the patients who will be re-hospitalized within
#500 days or not

pats=f2$COD_REG
reosp500=f2$reosp
for(i in c(1:length(pats))){
  pat=pats[i]
  temp=subset(ospedalizzazioni,COD_REG==pat)
  n=dim(temp)[1]
  reosp500[i]=0
  if(n>1 && temp$diff_time[2]<=500)
    reosp500[i]=1
  
}
f2$reosp=reosp500


f2$reosp=as.factor(f2$reosp)
df=f2[-c(1:1000),]
df_test=f2[c(1:1000),]

#Overview on the relation between the time passed in hospital and the
#label of the re-hospitalization
table(df$qt_prest_Sum)
perc1=NULL
div=c(1,5,6,10,11,96) #0-5 6-10 11-
temp=mean(as.numeric(subset(df, qt_prest_Sum<=5)$reosp)-1)
perc1=c(perc1, temp)
temp1=subset(df, qt_prest_Sum<=10)
temp=mean(as.numeric(subset(temp1, qt_prest_Sum>5)$reosp)-1)
perc1=c(perc1, temp)
temp=mean(as.numeric(subset(df, qt_prest_Sum>10)$reosp)-1)
perc1=c(perc1, temp)
perc1


#######################
#Logistic Linear Model
#######################

#We started with a logistic regression about the survival probability
#We created a mixed effects linear model, considering the hospital effect
#as a random intercept.

#Response: 

#reosp        #1 re-hospitalized, 0 no


#Regressors: 

#scale(eta_Min)        
#log(qt_prest_Sum)      #time passed in hospital (in the first hospitalization)
#grado                  #fragility grade, based on the first diagnosis
#num_com                #number of comorbidities
#(1|strutt_id)          #hospital random intercept

l4=glmer(reosp ~ scale(eta_Min)+ log(qt_prest_Sum)+grado+num_com+(1|strutt_id),family = binomial, data=df)
summary(l4)
mod=l4

x11()
dotplot(ranef(mod))
sigma2_b <- as.numeric(get_variance_random(mod))
VPC <- sigma2_b/(sigma2_b+pi*pi/3)  #sigma2_b/(sigma2_b+pi*pi/3)
VPC

#1% -> with a single hospitalization the effects of the hospital are not evident
#and we don't have a clear view of the patient (single diagnosis).


##################################
#Evaluating goodness of the model
##################################

#We prefer specificity over sensibility: it's more important to know
#if a patient could be re-hospitalized, to give more attention to him.

soglia=0.45
fit=fitted.values(mod)
fit_bin=rep(0,dim(df)[1])
fit_bin[which(fit>soglia)]=1
ver=df$reosp
ver_bin=rep(0,dim(df)[1])
ver_bin[which(ver==1)]=1
tab=table(fit_bin,ver_bin)
tab

soglia=0.45
fut=predict(mod,df_test,allow.new.levels = T,type="response")
fut_bin=rep(0,length(fut))
fut_bin[which(fut>soglia)]=1
ver=df_test$reosp
ver_bin=rep(0,dim(df_test)[1])
ver_bin[which(ver==1)]=1
tab=table(fut_bin,ver_bin)
tab
sensibilita =  tab [ 2,2 ] /( tab [ 1,2 ] + tab [2,2] ) 
sensibilita
specificita = tab[ 1,1 ] /( tab [ 1,1 ] + tab [ 2,1 ] )
specificita

pred <-prediction(fut, ver)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR


#The doctor has an idea of the days necessary to treat a patient during the 
#first hospitalization. This model can help to make sure the patient can
#be sent home after this period: a patient that could possibly be
#re-hospitalized will need more attention instead of one that could be treat at
#home.
#_______________________________________________________________________________





################################################################################
#-------------------------------------------------------------------------------
#Duration of a Single Hospitalization
#-------------------------------------------------------------------------------
################################################################################


#We continued considering the hospitalizations, this time we put our
#attention on the total time passed in hospital during a single hospitalization.

#We decided to study the hospitalization length to help a doctor in the decision 
# making, integrating new information with the other models and to suggest the
#hospital in the organization of the time to reserve rooms for the patients.

#This time we used all the information of the hospitalizations over three years,
#since the first ones for each patient.
#We considered the patients studied at least 2 years

rm(list=ls())
graphics.off()

load("C:/Users/franc/Desktop/poli/4 anno/applied stat/progetto/ggosp/ospedalizzazioni_ccs_time_cumulative.Rda")
df = ospedalizzazioni
set.seed(1)

#We considered the patients studied at least 2 years
pat = unique(ospedalizzazioni[which(ospedalizzazioni$cumulative_time>=730),]$COD_REG)
data = subset(ospedalizzazioni, COD_REG %in% pat)
length(unique(df$COD_REG))
#reduce dimensionality, taking 1000 patients
paz = sample(unique(df$COD_REG),1000)
df = subset(df,COD_REG %in% paz)
df_test=subset(ospedalizzazioni,!(COD_REG %in% paz))
paz1=sample(unique(df_test$COD_REG),500)
df_test = subset(df_test,COD_REG %in% paz1)

#We grouped by total time passed in hospital (0, 1-10, >10)
#To make then a plot of the residuals
table(df$total_time_osp)
col=df$total_time_osp
j0=which(df$total_time_osp ==0)
j1=which(df$total_time_osp %in% c(1:10))
j2=which(df$total_time_osp %in% c(11:30))
j3=which(df$total_time_osp >30)
set.seed(1)
colori =rainbow(3)
col[j0]=colori[1]
col[j1]=colori[2]
j2=c(j2,j3)
col[j2]=colori[3]


###########################
#Mixed Effect Linear Model
###########################

#We introduced a nested mixed effects model taking into account the fact that
#a patient could have more hospitalizations in the same hospital. We considered
#the heteroskedasticity of the residuals, since we observed a different 
#variability with the time going on: patients have different treatment path and
#can have a different disease evolution.

#Response: 

#log(qt_prest_Sum)      #duration of an hospitalization


#Regressors: 

#log(eta_Min)        
#log(cumulative_time))  #cumulative time passed in hospital 
#grado                  #fragility grade, based on the first diagnosis
#num_com                #number of comorbidities
#(1|strutt_id/COD_REG)  #nested model: hospital(patient)


l4=lme(log(qt_prest_Sum)~ log(eta_Min)+log(cumulative_time)+grado+num_com,random = ~1|strutt_id/COD_REG,data=df) 
mod=l4
x11()
plot(residuals(mod,type = c("response")),main='time', col=col, ylab='residuals')
abline(h=0)
legend(800, 2.5, legend=c("0", "1:10", ">10"),
       col=colori, lty=1, cex=0.8)

#Residuals corresponding to different total time passed in hospital have different
#variance. The ones with a greater time in hospital have greater variability.

j0=which(df$total_time_osp ==0)
j1=which(df$total_time_osp %in% c(1:10))
j2=which(df$total_time_osp >10)
df$group_time=0
df[j0,'group_time']=0
df[j1,'group_time']=1
df[j2,'group_time']=2

#We updated the model considering the grouping by time passed in hospital
l42 <- update(l4,
              weights = varIdent(form = ~1|group_time), 
              data = df)
summary(l42)
mod=l42
r2(mod)

#Verify assumptions
mod=l42
x11()
#1)
qqnorm(resid(mod))
qqline(resid(mod), col='red', lwd=2)
# 2) Assessing Assumption on the Random Effects
x11()
#hospital
qqnorm(unlist(ranef(mod)$strutt_id[,1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(mod)$strutt_id[,1]), col='red', lwd=2)
x11()
#patient
qqnorm(unlist(ranef(mod)$COD_REG[,1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(mod)$COD_REG[,1]), col='red', lwd=2)

#random
mod=l42
summary(mod)
#hospital
r_int_osp=ranef(mod)[1]$strutt_id
x11()
plot(sort(r_int_osp[,1]),c(1:dim(r_int_osp)[1]))
#patient
r_int_pat=ranef(mod)[2]$COD_REG
x11()
plot(sort(r_int_pat[,1]),c(1:dim(r_int_pat)[1]))

# From the summary we look for the variance of the residuals and of the
#nested effects
sigma2_eps <- 0.5646^2#as.numeric(get_variance_residual(mod))
sigma2_b <- 0.16292^2 + 0.3001^2
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE


s2pa=0.3001^2
s2osp=0.1629262^2
s2res <- 0.5646342^2#as.numeric(get_variance_residual(mod))
PVRE_p=s2pa/(s2pa+s2osp+s2res)
PVRE_o=s2osp/(s2pa+s2osp+s2res)
PVRE_p
PVRE_o


#The model can't capture enough variability to entrust it, because we don't
#have information on the evolution of the patients problems, but can be used as
# an advise tool.





