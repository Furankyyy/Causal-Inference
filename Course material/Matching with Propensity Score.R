library(foreign)

RCT_data <- read.dta("http://www.nber.org/~rdehejia/data/nsw.dta")

RCT_diffmeans <- 	mean(RCT_data$re78[RCT_data$treat == 1]) - 
  mean(RCT_data$re78[RCT_data$treat == 0])
print(RCT_diffmeans)
summary(lm(RCT_data$re78 ~ RCT_data$treat)) # note the coef on “treat”
confint(lm(RCT_data$re78 ~ RCT_data$treat)) # note the confint on “treat”




foo <- read.csv("dw_data.csv")

library(Matching)

model <- glm(treat~age+I(age^2)+education+black+hispanic+married+nodegree+
               re74+I(re74^2)+re75+I(re75^2),data=foo,family=binomial)

X <- model$fitted.values
Y <- foo$re78
Tr <- foo$treat

rr <- Match(Y=Y,Tr=Tr,X=X,M=1)


mb <- MatchBalance(treat~age+I(age^2)+education+black+hispanic+married
                   +nodegree+re74+I(re74^2)+re75+I(re75^2),data=foo,
                   match.out = rr,nboots=10)

