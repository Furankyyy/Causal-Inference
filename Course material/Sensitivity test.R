library(MASS)
library(Matching)
library(rbounds)
library(foreign)

foo <- read.csv("dw_data.csv")

model <- glm(treat~age+I(age^2)+education+black+hispanic+married+nodegree+
               re74+I(re74^2)+re75+I(re75^2),data=foo,family=binomial)

X <- model$fitted.values
Y <- foo$re78
Tr <- foo$treat

rr <- Match(Y=Y,Tr=Tr,X=X,M=1)


mb <- MatchBalance(treat~age+I(age^2)+education+black+hispanic+married
                   +nodegree+re74+I(re74^2)+re75+I(re75^2),data=foo,
                   match.out = rr,nboots=10)

psens(rr,Gamma=2,GammaInc=0.2)

hlsens(rr,pr=.5,Gamma=2,GammaInc=0.2)
