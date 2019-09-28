library(foreign)

foo<-read.dta("jtpa.dta")

##1.If the assignment to treatment and control group were truly random, we would not expect to 
#see any systematic differences between the treatment and control groups on the baseline 
#characteristics. Test whether you see any differences between treatment and control in 2 baseline 
#characteristics, such as race, previous work experience, marital status, age, etc.

t.test(foo$age[which(foo$assignmt==1)],foo$age[which(foo$assignmt==0)])
#p-value > 0.05. Can't reject the null hypothesis. No difference in age between the groups.

t.test(foo$prevearn[which(foo$assignmt==1)],foo$prevearn[which(foo$assignmt==0)])
#p-value > 0.05. Can't reject the null hypothesis. No difference in previous earnings between 
#the groups.


##2. Determine whether or not this was an experiment with perfect compliance.

which(foo$training[which(foo$assignmt==0)]==1)
which(foo$training[which(foo$assignmt==1)]==0)

#No Perfect Compliance.


##3.What is your estimate of the impact of assignment to the JTPA program on workers’ earnings during 
#30 months after random assignment?

en_treat <- length(which(foo$training[which(foo$assignmt==1)]==1))/length(which(foo$assignmt==1))
nonen_treat <- length(which(foo$training[which(foo$assignmt==0)]==1))/length(which(foo$assignmt==0))
ITT <- mean(foo$earnings[which(foo$assignmt==1)])-mean(foo$earnings[which(foo$assignmt==0)])
comp_rate <- en_treat - nonen_treat
late <- ITT/comp_rate

#Wald estimate is 1848.829


###ivreg and ivmodel

library(AER);library(ivmodel)

ivreg(earnings~training|assignmt,data=foo)

ivmodel(Y=foo$earnings,D=foo$training,Z=foo$assignmt,deltarange = c(-0.03,0.03))

