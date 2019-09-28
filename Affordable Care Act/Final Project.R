####################################################################################
#
#Replication for: Young Invincibles to Young Insurables: Insurance uptake by the healthy, wealthy and employed
#
####################################################################################

library(AER)
library(arm)
library(foreign)
library(plyr)
library(sandwich)
library(car)
library(zoo)
library(xtable)
library(ggplot2)
library(scales)
library(gridExtra)
library(Matching)
library(rgenoud)
library(MASS)

#Dataset
data<-read.dta("depcov_dataset1.dta")


####################################################################################
##
# DATA PREPARATION
##
####################################################################################


##Generate Age Dummies

for(i in 16:25){
  name<-paste("age",i,sep="")
  data[,name]<-as.integer(data$age==i)}

for(i in 27:29){
  name<-paste("age",i,sep="")
  data[,name]<-as.integer(data$age==i)}

##Year Dummies
for(i in 2009:2011){
  name<-paste("y",i,sep="")
  data[,name]<-as.integer(data$year==i)}

##Month Dummies
data$month<-as.integer(data$month)
for(i in 1:12){
  name<-paste("month",i,sep="")
  data[,name]<-as.integer(data$month==i)}

rm(name, i)

##Trend Variable
data$trend<-(data$year-2008)*12+(data$month-8)
data$trend2 <- data$trend^2

##Post reform dummies
data$mar_sept10<-as.integer(data$year==2010 & data$month>=3 & data$month<=9)
data$after_oct10<-as.integer((data$year==2010 & data$month>=10) | data$year>=2011)
data$oct10_feb11<-as.integer((data$year==2010 & data$month>=10) | (data$year==2011 & data$month<=2))
data$mar11_sept11<-as.integer(data$year==2011 & data$month>=3 & data$month<=9)
data$after_sept11<-as.integer((data$year==2011 & data$month>9) | data$year>2011)
data$after_mar10<-as.integer((data$year==2010 & data$month>=3) | data$year>=2011)

#Treatment group dummy
data$fedelig<-as.integer(data$age>=19 & data$age<26)

#Interactions Post Reform and Treatment Group
data$elig_mar10<-data$mar_sept10*data$fedelig
data$elig_oct10<-data$after_oct10*data$fedelig
data$elig_middle<-data$oct10_feb11*data$fedelig
data$elig_marsept<-data$mar11_sept11*data$fedelig
data$elig_sept11<-data$after_sept11*data$fedelig

#Splined variables
data$trendbefore_mar10        <- (1-data$after_mar10)*data$trend
data$trendafter_mar10         <- data$after_mar10*((data$year-2010)*12+(data$month-3))

data$trendbefore_mar10_treat  <- (1-data$after_mar10)*data$trend*data$fedelig
data$trendafter_mar10_treat   <- data$after_mar10*((data$year-2010)*12+(data$month-3))*data$fedelig

data$before_mar10 <- as.integer((data$year==2010 & data$month<3) | data$year<=2009)
data$before_mar10_treat <- data$before_mar10*data$fedelig
data$after_mar10_treat <- data$before_mar10*data$fedelig

#Interactions Unempoyment Rate and Treatment group
data$ue_treat<-data$ue*data$fedelig

##Group Variables
data$age_group<-NA
data[data$age>=16 & data$age<=18,"age_group"]<-1
data[data$age>=19 & data$age<=25,"age_group"]<-2
data[data$age>=27 & data$age<=29,"age_group"]<-3

###Add State trend
v<-sort(unique(data$fipstate))

for(i in 1:length(v)){
  name<-paste("st_trend_",i,sep="")
  data[,name]<-0
  data[data$fipstate==v[i],name]<-data[data$fipstate==v[i],]$trend
}
rm(i, name, v)
### Add State2 trend
v<-sort(unique(data$fipstate))
for(i in 1:length(v)){
  name<-paste("st_trend2_",i,sep="")
  data[,name]<-0
  data[data$fipstate==v[i],name]<-data[data$fipstate==v[i],]$trend2
}
rm(i, name, v)

data$trend_treat<-data$trend*data$fedelig

data$unins<-as.integer(data$anyhi==0)



####################################################################################
##
# REPLICATION - TABLE 1 AND TABLE 3
##
####################################################################################



####################################################################################
# Table 1 Replication
####################################################################################

#Table 1

Table1Var<-c("anyhi","emphi_dep","indiv","emphi","govhi", "employed", "unemployed", "age", "white", "black", "hispanic", "mar", "ft_student","hsdo","hsg", "somcol", "colgrd", "bad_hlth", "famyincy")
Names<-c("Covered by any health insurance (HI)", "Covered as parent's dependent", "Covered by own employer HI", "Covered by individually purchased HI in own name", "Covered by government HI", "Employed", "Unemployed","Age", "White", "African-American", "Hispanic", "Married", "Student", "Less than high school", "High school graduate", "Some college", "College graduate", "Self-reported health is less than excellent", "Average family yearly income", "Number of person-month observations", "Corresponding number of unique persons")

Table1<-matrix(0,length(Names), 4)
rownames(Table1)<-Names
colnames(Table1)<-c("All", "Age 16-18", "Age 19-25", "Age 27-29")

for(i in 1: length(Table1Var)){
  
  Table1[i,1]<-round(weighted.mean(data[,Table1Var[i]],data$p_weight, na.rm=T),digits=3)
  Table1[i,2]<-round(weighted.mean(data[data$age_group==1,Table1Var[i]],data[data$age_group==1,]$p_weight, na.rm=T),digits=3)
  Table1[i,3]<-round(weighted.mean(data[data$age_group==2,Table1Var[i]],data[data$age_group==2,]$p_weight, na.rm=T),digits=3)
  Table1[i,4]<-round(weighted.mean(data[data$age_group==3,Table1Var[i]],data[data$age_group==3,]$p_weight, na.rm=T),digits=3)
}

Table1[20,1]<-round(nrow(data), digits=0)
Table1[20,2]<-round(sum(as.integer(data$age_group==1)), digits=0)
Table1[20,3]<-round(sum(as.integer(data$age_group==2)), digits=0)
Table1[20,4]<-round(sum(as.integer(data$age_group==3)), digits=0)
Table1[21,1]<-round(length(unique(data$groupid)), digits=0)
Table1[21,2]<-round(length(unique(data[data$age_group==1,]$groupid)), digits=0)
Table1[21,3]<-round(length(unique(data[data$age_group==2,]$groupid)), digits=0)
Table1[21,4]<-round(length(unique(data[data$age_group==3,]$groupid)), digits=0)

rm(i)
Table1

write.csv(file="Descriptive Results Table (Table 1).csv",x=Table1)


####################################################################################
# Table 3 Replication
####################################################################################

### REGRESSIONS VARIABLES

Outcomes<-subset(data, select=c(anyhi, emphi_dep, indiv, emphi, govhi))
Effects<-as.matrix(subset(data, select=c(mar_sept10, after_oct10, fedelig, elig_mar10, elig_oct10)))
EffectsSplineRates <- as.matrix(subset(data, select=c(trendafter_mar10,trendafter_mar10_treat)))
EffectsSplineLevels <- as.matrix(subset(data, select=c(after_mar10,after_mar10_treat)))
ExpVars<-as.matrix(subset(data, select=c(age17, age18, grep("age2", names(data)),trend, female, ue, hispanic, white, asian, other, student, mar, grep("fpl_ratio", names(data)), ue_treat, y2009, y2010, month2, month3, month4, month5, month6, month7, month8, month9, month10, month11, month12, grep("st_trend_", names(data)))))
ExpVarsNoTime  <- as.matrix(subset(data, select=c(fedelig,age17,age18,age19,age20,age21,age22,age23,age24,age25,age27,age28,age29,female,hispanic,white,asian,other,student,mar,ue,fpl_ratio,fpl_ratio_2,ue_treat)))
StateTrends<-as.matrix(subset(data, select=c(grep("st_trend_", names(data)))))
StateTrend2s<-as.matrix(subset(data, select=c(grep("st_trend2_", names(data)))))


#Cluster robust inference
cl   <- function(data,model,cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  Robust<-as.matrix(coeftest(model, vcovCL))
  return(Robust) }


#Table 3

Table3_rep <- matrix("",7,1)
colnames(Table3_rep)<-c("Table 3 Column 9 Replication - Modified Model")
rownames(Table3_rep)<-c("Eligible","After March 10", "After Mar'10*Eligible", "Trend after Mar'10", "Trend after Mar'10*Eligible","State linear trends","State linear trends^2")
y <- data$anyhi
Results2 <- cl(data,lm(y~data$fedelig+EffectsSplineLevels+data$trendafter_mar10+data$trendafter_mar10_treat+StateTrends+StateTrend2s+ExpVarsNoTime+factor(data$fipstate), w=data$p_weight), data$fipstate)

stars1<-stars2<-stars3<-stars4<-stars5<-as.character(NA)

if (Results2[2,4]<=0.01) stars1<-"***" else if (Results2[2,4]<=0.05 & Results2[2,4]>0.01) stars1<-"**" else if (Results2[2,4]<=0.10 & Results2[2,4]>0.05) stars1<-"*" else stars1<-""
if (Results2[3,4]<=0.01) stars2<-"***" else if (Results2[3,4]<=0.05 & Results2[3,4]>0.01) stars2<-"**" else if (Results2[3,4]<=0.10 & Results2[3,4]>0.05) stars2<-"*" else stars2<-""
if (Results2[4,4]<=0.01) stars3<-"***" else if (Results2[4,4]<=0.05 & Results2[4,4]>0.01) stars3<-"**" else if (Results2[4,4]<=0.10 & Results2[4,4]>0.05) stars3<-"*" else stars3<-""
if (Results2[5,4]<=0.01) stars4<-"***" else if (Results2[5,4]<=0.05 & Results2[5,4]>0.01) stars4<-"**" else if (Results2[5,4]<=0.10 & Results2[5,4]>0.05) stars4<-"*" else stars4<-""
if (Results2[6,4]<=0.01) stars5<-"***" else if (Results2[6,4]<=0.05 & Results2[6,4]>0.01) stars5<-"**" else if (Results2[6,4]<=0.10 & Results2[6,4]>0.05) stars5<-"*" else stars5<-""


Table3_rep[1,1]<-paste(round(Results2[2,1], digits=4), " ","(",(round(Results2[2,2], digits=4)),")", stars1, sep="") 
Table3_rep[2,1]<-paste(round(Results2[3,1], digits=4), " ","(",(round(Results2[3,2], digits=4)),")", stars2, sep="")                   
Table3_rep[3,1]<-paste(round(Results2[4,1], digits=4), " ","(",(round(Results2[4,2], digits=4)),")", stars3, sep="")  
Table3_rep[4,1]<-paste(round(Results2[5,1], digits=4), " ","(",(round(Results2[5,2], digits=4)),")", stars4, sep="")                   
Table3_rep[5,1]<-paste(round(Results2[6,1], digits=4), " ","(",(round(Results2[6,2], digits=4)),")", stars5, sep="")                   
Table3_rep[6,1]<-"Yes"
Table3_rep[7,1]<-"Yes"

Table3_rep

write.csv(file="General Results Table (Table 3).csv", x=Table3_rep)




####################################################################################
##
# EXTENSION
##
####################################################################################



####################################################################################
# Matching
####################################################################################
age16_18 <- subset(data,data$age>=16 & data$age<19)
x <- rep(0,length(age16_18$age))
age16_18 <- cbind(age16_18,x)
age18_20 <- subset(data,data$age>=19 & data$age<21)
x <- rep(1,length(age18_20$age))
age18_20 <- cbind(age18_20,x)

newdataset<-rbind(age16_18,age18_20)
newdataset<-newdataset[-which(is.na(newdataset$health)),]


#Exact matching
matchvar_exact <- cbind(newdataset$female,newdataset$health,newdataset$hispanic,newdataset$white,newdataset$asian,newdataset$other,newdataset$student,
                                    newdataset$ue,newdataset$st_with_law)
exact <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)

gen_exact <- GenMatch(Tr=newdataset$x,X=matchvar_exact,pop.size=20,max.generation=15,wait.generations=5,ties = FALSE,exact=exact)

mout_exact <- Match(Tr=newdataset$x,X=matchvar_exact,Weight.matrix = gen_exact,ties = FALSE,exact=exact)

mb_exact <- MatchBalance(x~female + health + hispanic + white + asian+other+student+ue+st_with_law, 
                                     data=newdataset, match.out = mout, nboots=200)


#Exact and caliper matching with caliper = 0.25
matchvar_exact_caliper0.25 <- cbind(newdataset$female,newdataset$health,newdataset$hispanic,newdataset$white,newdataset$asian,newdataset$other,newdataset$student,
                newdataset$ue,newdataset$st_with_law,newdataset$famyinc)
exact_caliper0.25 <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE)

gen_exact_caliper0.25 <- GenMatch(Tr=newdataset$x,X=matchvar_exact_caliper0.25,pop.size=20,max.generation=15,wait.generations=5,ties = FALSE,exact=exact,caliper=.25)

mout_exact_caliper0.25 <- Match(Tr=newdataset$x,X=matchvar_exact_caliper0.25,Weight.matrix = gen_exact_caliper0.25,ties = FALSE,exact=exact_caliper0.25,caliper=.25)

mb_exact_caliper0.25 <- MatchBalance(x~female + health + hispanic + white + asian+other+student+ue+st_with_law+famyinc, 
                       data=newdataset, match.out = mout_exact_caliper0.25, nboots=200)


#Exact and caliper matching with caliper = 0.1
matchvar_exact_caliper0.1 <- cbind(newdataset$female,newdataset$health,newdataset$hispanic,newdataset$white,newdataset$asian,newdataset$other,newdataset$student,
                                    newdataset$ue,newdataset$st_with_law,newdataset$famyinc)
exact_caliper0.1 <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE)

gen_exact_caliper0.1 <- GenMatch(Tr=newdataset$x,X=matchvar_exact_caliper0.1,pop.size=20,max.generation=15,wait.generations=5,ties = FALSE,exact=exact_caliper0.1,caliper=.1)

mout_exact_caliper0.1 <- Match(Tr=newdataset$x,X=matchvar_exact_caliper0.1,Weight.matrix = gen_exact_caliper0.1,ties = FALSE,exact=exact_caliper0.1,caliper=.1)

mb_exact_caliper0.1 <- MatchBalance(x~female + health + hispanic + white + asian+other+student+ue+st_with_law+famyinc, 
                                     data=newdataset, match.out = mout_exact_caliper0.1, nboots=200)


#Check key variable difference between dropped data and used data in mout_exact_caliper0.1
t.test(matchvar_exact_caliper0.1[mout_exact_caliper0.1$index.dropped,1],matchvar_exact_caliper0.1[-mout_exact_caliper0.1$index.dropped,1])
t.test(matchvar_exact_caliper0.1[mout_exact_caliper0.1$index.dropped,8],matchvar_exact_caliper0.1[-mout_exact_caliper0.1$index.dropped,8]) 
t.test(matchvar_exact_caliper0.1[mout_exact_caliper0.1$index.dropped,10],matchvar_exact_caliper0.1[-mout_exact_caliper0.1$index.dropped,10])


####################################################################################
# Regression Discontinuity, with cutoff point at age=19
####################################################################################


#Divide the matched dataset
new_control <- newdataset[mout_exact_caliper0.1$index.control,]
new_treat <- newdataset[mout_exact_caliper0.1$index.treated,]

#Logistic regression based on control data
glm1 <- glm(data=new_control,anyhi~female + health + hispanic + white + asian+other+student+ue+st_with_law+famyinc+age,family="binomial")

#Obtain confidence interval and two sample difference in means T-test statistics through simulation of probabilities at age=19, while holding
#all other variables to the mean (setting health status to its mode, "Good", since it is a factor variable)

#Confidence interval for control data
sim_coef <- sim(glm1,n.sim=5000)
logit1 <- rep(0,5000)
for(i in 1:5000){
  logit1[i]<-sum(c(1,mean(newdataset$female),0,1,0,0,mean(newdataset$hispanic),mean(newdataset$white),mean(newdataset$asian),
               mean(newdataset$other),mean(newdataset$student),mean(newdataset$ue),mean(newdataset$st_with_law),mean(newdataset$famyinc),
               19)*sim_coef@coef[i,],rnorm(1,0,sim_coef@sigma[i]))
}

prob_control<-exp(logit1)/(1+exp(logit1))
conf_int<-quantile(prob_control,c(0.025,0.975))


#Logistic regression based on treated data
glm2 <- glm(data=new_treat,anyhi~female + health + hispanic + white + asian+other+student+ue+st_with_law+famyinc+age,family="binomial")

#Confidence interval for treated data
sim_coef2 <- sim(glm2,n.sim=5000)
logit2 <- rep(0,5000)
for(i in 1:5000){
  logit2[i]<-sum(c(1,mean(newdataset$female),0,1,0,0,mean(newdataset$hispanic),mean(newdataset$white),mean(newdataset$asian),
                   mean(newdataset$other),mean(newdataset$student),mean(newdataset$ue),mean(newdataset$st_with_law),mean(newdataset$famyinc),
                   19)*sim_coef@coef[i,],rnorm(1,0,sim_coef@sigma[i]))
}

prob_treat<-exp(logit2)/(1+exp(logit2))
conf_int2<-quantile(prob_treat,c(0.025,0.975))

#Result, the two sample p-value and confidence interval of treatment effect at cutoff point age=19
t.test(prob_control,prob_treat)

