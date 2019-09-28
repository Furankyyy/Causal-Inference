###CS112 Causal Inference Assignment###
library(Matching)
library(rgenoud)
library(rbounds)
library(foreign)
library(MASS)


##(1)

##(a)
#The error is that the code misses the Match() function. GenMatch() only returns the weight matrix, which need to be passed on
#to Match() to do the actual matching to produce mout. Also, the match.out argument in MatchBalance should use the matching 
#result, which is mout (produced by Match() function) instead of using the weights, genout.

##(b)
#The estimand in GenMatch() is specified as ATE. However, the estimand argument is not specified in Match(), which means it is 
#set to its default, ATT. The estimand in the two functions are different, so if we use the genout in Match(), the result will
#be flawed. The estimand in Match() should also be set to ATE.

##(c)
#The M argument in Match() is specified to be 2. However, the M in GenMatch() is not specified, which means that it is set to 
#its default, M=1. Therefore, GenMatch() produces the weight matrix it finds for matching each treatment unit with 1 control
#unit, but the Match() function uses this weight matrix to match each treatment unit with 2 control units, hence producing a 
#flawed result. The M argument in GenMatch should also be set to M=2.


##(2)
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

#Extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

#Remove 2 rows with missing data (I think we need to check whether missing data is correlated with certain covariates or whether
#there is any pattern within the missing data, and we can potentially fill out the missing data with predicted data. But for 
#assignment purpose I did not further explore this)
foo <- foo[c(-19, -47), ]

#Check that all missing data is gone
which(is.na(foo) == TRUE)

#Logistic regression of the original model
glm1 <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap+develop+exp+decade+treaty+untype4,data=foo, 
            family="binomial")

#Logistic regression of the modified model
glm2 <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap+develop+exp+decade+treaty+untype4+I(untype4*wardur),
            data=foo, family="binomial")

#Calculate marginal effects for glm1. Marginal effect is defined as the difference in outcome when the treatment variable is 1
#and 0.
logit1 <- rep(0,length(foo$untype4))
for (i in 1:length(foo$untype4)){
  logit1[i] <- sum(c(1,mean(foo$wartype),mean(foo$logcost),foo$wardur[i],mean(foo$factnum),mean(foo$factnum2),mean(foo$trnsfcap),
                 mean(foo$develop),mean(foo$exp),mean(foo$decade),mean(foo$treaty),foo$untype4[i]) * glm1$coefficients)
}
effect.logit1 <- exp(logit1)/(1+exp(logit1))

logit1.counterfactual <- rep(0,length(foo$untype4))
for (i in 1:length(foo$untype4)){
  logit1.counterfactual[i] <- sum(c(1,mean(foo$wartype),mean(foo$logcost),foo$wardur[i],mean(foo$factnum),mean(foo$factnum2),
                                    mean(foo$trnsfcap),mean(foo$develop),mean(foo$exp),mean(foo$decade),mean(foo$treaty),
                                    1-foo$untype4[i]) * glm1$coefficients)
}
effect.counterfactual1 <- exp(logit1.counterfactual)/(1+exp(logit1.counterfactual))

#It is not surprised to find that the below two lines of code return the same outcome.
which(effect.logit1>effect.counterfactual1)
which(foo$untype4==1)

#This means that all treatment effects are positive. It saves me a lot of steps to sort the dataset into Y|X=1 and Y|X=0,
#because the marginal effect can be calculated by finding the absolute difference in effect.logit1 and effect.counterfactual1
marginal.effect.glm1 <- abs(effect.logit1 - effect.counterfactual1)

df1 <- data.frame(foo$wardur,marginal.effect.glm1)

#Same strategy to calculate marginal effects for glm1.
logit2 <- rep(0,length(foo$untype4))
for (i in 1:length(foo$untype4)){
  logit2[i] <- sum(c(1,mean(foo$wartype),mean(foo$logcost),foo$wardur[i],mean(foo$factnum),mean(foo$factnum2),mean(foo$trnsfcap),
                     mean(foo$develop),mean(foo$exp),mean(foo$decade),mean(foo$treaty),foo$untype4[i],
                     foo$untype4[i]*foo$wardur[i]) * glm2$coefficients)
}
effect.logit2 <- exp(logit2)/(1+exp(logit2))

logit2.counterfactual <- rep(0,length(foo$untype4))
for (i in 1:length(foo$untype4)){
  logit2.counterfactual[i] <- sum(c(1,mean(foo$wartype),mean(foo$logcost),foo$wardur[i],mean(foo$factnum),mean(foo$factnum2),
                                    mean(foo$trnsfcap),mean(foo$develop),mean(foo$exp),mean(foo$decade),mean(foo$treaty),
                                    1-foo$untype4[i],(1-foo$untype4[i])*foo$wardur[i]) * glm2$coefficients)
}
effect.counterfactual2 <- exp(logit2.counterfactual)/(1+exp(logit2.counterfactual))

#Same test above showing that treatment effect is positive.
which(effect.logit2>effect.counterfactual2)
which(foo$untype4==1)

marginal.effect.glm2 <- abs(effect.logit2 - effect.counterfactual2)

df2 <- data.frame(foo$wardur,marginal.effect.glm2)

#Plot the marginal effects.
sp1 <- spline(x=df1$foo.wardur,y=df1$marginal.effect.glm1)
sp2 <- spline(x=df2$foo.wardur,y=df2$marginal.effect.glm2)
plot(sp1,xlim=c(0,315),ylim=c(0,0.8),type="l",lty=2,sub="FIG.8.Causal Effect of Multidimensional UN Peacekeeping Operations",
     xlab="Duration of wars in months",ylab="Marginal effects of UN peacekeeping operations",font.sub=2)
lines(sp2,xlim=c(0,315),ylim=c(0,0.8),type="l",lty=1)


##(3)

#This treatment here is defined as any kind of peacekeeping intervention (observer, tranditional PKO, peace enforcement, or 
#multidimensional PKO). The control is that the war did not receive any type of intervention. The code creates a vector, 
#where wars that received peacekeeping operations (of any kind) are 1s and wars that did not receive intervetion are 0s.


##(4)

#(a)
#Causal inference: Do UN peace operations (any kind of UN intervention: deployment of observer, multidimensional
#peacekeeping, traditional peacekeeping, or peace enforcement) affect the success of peacebuilding in 2 years/5 years of time
#after the war starts versus non-UN-interventions of any kind?

#(b)
#There can be correlation (or causation) between two or several conflicts (e.g.Iraq/Kurds and Iraq/Shiites), especially when 
#the geographical and diplomatic relationship between the countries in conflict is very close and/or one country is 
#participating in two conflicts, so the UN operation in one conflict may affect the outcome in another.

#The restrict argument can restrict the possible matches, making two units impossible (or harder) to match no matter how
#similar their covariates are, so that the effect of assignment mechanism on outcomes can be mitigated (e.g. restricting
#China/Taiwan from matching with China/Tibet since the assignment in one may affect the outcome of the other)

#(c)

#Reload the data
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

#Assign treatment and controls
Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1

foo <- cbind(foo,Tr)

#Independent variables: Tr, wartype, logcost, wardur, factnum, factnum2, trnsfcap, treaty, develop, exp, decade,
#major,dead, wardur*dead
#Dependent variable(s): pbs2l,pbs5l

#Convert variable to numeric numbers 0 and 1
foo$pbs2l <- as.numeric(foo$pbs2l)-1
foo$pbs5l <- as.numeric(foo$pbs5l)-1

#For two years: pbs2l

#Remove rows of missing data
foo <- foo[c(-19, -47), ]

#Method 1: Logistic regression
glm.log.2yrs <- glm(pbs2l~Tr+wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                  wardur*dead,data=foo,family="binomial")

newdata1 <- foo
newdata1$Tr <- 1-newdata1$Tr

counterfactual.2yrs <- predict.glm(glm.log.2yrs,newdata=newdata1,type=c("response"))

ATT.2yrs <- mean(foo$pbs2l[which(foo$Tr==1)]) - mean(counterfactual.2yrs[which(foo$Tr==1)])

#Method 2: Propensity score matching
glm.pscore.2yrs <- glm(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                         wardur*dead,data=foo,family="binomial")

mout.pscore.2yrs <- Match(Tr=foo$Tr, X=glm.pscore.2yrs$fitted)

mb.pscore.2yrs <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                                 wardur*dead,data=foo,match.out=mout.pscore.2yrs,nboots=500)

mout.pscore.2yrs.effect <- Match(Y=foo$pbs2l,Tr=foo$Tr, X=glm.pscore.2yrs$fitted,BiasAdjust = TRUE)

#Method 3: Genetic matching
X.2yrs <- cbind(foo$wartype,foo$logcost,foo$wardur,foo$factnum,foo$factnum2,foo$trnsfcap,foo$treaty,foo$develop,foo$exp,
                foo$decade,foo$major,foo$dead,foo$wardur*foo$dead)

genout.2yrs <- GenMatch(Tr=foo$Tr,X=X.2yrs,pop.size=500,max.generation=50,wait.generations=10)

mout.gen.2yrs <- Match(Tr=foo$Tr,X=X.2yrs,Weight.matrix=genout.2yrs)

mb.gen.2yrs <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                              wardur*dead,data=foo,match.out=mout.gen.2yrs,nboots=500)

mout.gen.2yrs.effect <- Match(Y=foo$pbs2l,Tr=foo$Tr,X=X.2yrs,Weight.matrix=genout.2yrs,BiasAdjust = TRUE)

#Next, find the new dataset for the treatment effect of 5 years

#Delete rows with missing data
foo.5yrs <- foo[c(-4,-16,-82,-91,-96),]

#5yrs

#Method 1: Logistic regression
glm.log.5yrs <- glm(pbs5l~Tr+wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                      wardur*dead,data=foo.5yrs,family="binomial")

newdata2 <- foo.5yrs
newdata2$Tr <- 1-newdata2$Tr

counterfactual.5yrs <- predict.glm(glm.log.5yrs,newdata=newdata2,type=c("response"))

ATT.5yrs <- mean(foo$pbs5l[which(foo.5yrs$Tr==1)]) - mean(counterfactual.5yrs[which(foo.5yrs$Tr==1)])

#Method 2: Propensity score matching
glm.pscore.5yrs <- glm(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                         wardur*dead,data=foo.5yrs,family="binomial")

mout.pscore.5yrs <- Match(Tr=foo.5yrs$Tr, X=glm.pscore.5yrs$fitted)

mb.pscore.5yrs <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                                 wardur*dead,data=foo.5yrs,match.out=mout.pscore.5yrs,nboots=500)

mout.pscore.5yrs.effect <- Match(Y=foo.5yrs$pbs5l,Tr=foo.5yrs$Tr, X=glm.pscore.5yrs$fitted,BiasAdjust = TRUE)

#Method 3: Genetic Matching
X.5yrs <- cbind(foo.5yrs$wartype,foo.5yrs$logcost,foo.5yrs$wardur,foo.5yrs$factnum,foo.5yrs$factnum2,foo.5yrs$trnsfcap,
                foo.5yrs$treaty,foo.5yrs$develop,foo.5yrs$exp,foo.5yrs$decade,foo.5yrs$major,foo.5yrs$dead,
                foo.5yrs$wardur*foo.5yrs$dead)

genout.5yrs <- GenMatch(Tr=foo.5yrs$Tr,X=X.5yrs,pop.size=500,max.generation=50,wait.generations=10)

mout.gen.5yrs <- Match(Tr=foo.5yrs$Tr,X=X.5yrs,Weight.matrix=genout.5yrs)

mb.gen.5yrs <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+major+dead+
                              wardur*dead,data=foo.5yrs,match.out=mout.gen.5yrs,nboots=500)

mout.gen.5yrs.effect <- Match(Y=foo.5yrs$pbs5l,Tr=foo.5yrs$Tr,X=X.5yrs,Weight.matrix=genout.5yrs,BiasAdjust = TRUE)

#Through trial and error, I find out that M=1 produces the best leximin p value, so I do not use other values of M.

#Produce the table with the results from the above (MatchBalance, mout$est, summary(glm),etc.) See the document.

