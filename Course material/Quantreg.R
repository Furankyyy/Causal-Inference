rm(list=ls())
library(Matching)
library(foreign)

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00087718-6996/dw-data.csv")

X = cbind(foo$age, foo$education, foo$black, foo$re75, foo$re74)

#The covariates we want to obtain balance on
BalanceMat <- cbind(foo$age, foo$education, foo$black, foo$re75, foo$re74)

#
#Let's call GenMatch() to find the optimal weight to give each
#covariate in 'X' so as we have achieved balance on the covariates in
#'BalanceMat'. This is only an example so we want GenMatch to be quick
#so the population size has been set to be only 16 via the 'pop.size'
#option. This is *WAY* too small for actual problems.
#For details see http://sekhon.berkeley.edu/papers/MatchingJSS.pdf.

# It may take a while!!! Notice that I set "nboots"...
genout <- GenMatch(Tr=foo$treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT",
                   pop.size=50, max.generations=10, wait.generations=5, nboots = 1000)


#
# Now that GenMatch() has found the optimal weights, let's estimate
# our causal effect of interest using those weights
#
mout <- Match(Tr=foo$treat, X=X, estimand="ATT", Weight.matrix=genout)

#                        
#Let's determine if balance has actually been obtained on the variables of interest
#                        
mb <- MatchBalance(foo$treat ~ foo$age + foo$education + foo$black + 
                     foo$re75 + foo$re74, match.out=mout, nboots=1000)

# Define the data subset of treated units?
tr.units <- foo[mout$index.treated,]

# Define the data subset of matched control units
matched.controls <- foo[mout$index.control,]

matched.data <- rbind(tr.units, matched.controls)

# QUANTILE REGRESSION METHOD 1 (TOTALLY NON-PARAMETRIC, FEWEST ASSUMPTIONS)
# Non-parametric estimates of quantile effects at all percentile/quantiles:
non.par.quantile.effects <-   
  quantile(tr.units$re78, probs = 1:99/100) - 
  quantile(matched.controls$re78, probs = 1:99/100)

# e.g., the 75th percentile treatment effect is estimated to be:
non.par.quantile.effects[75]
# $3012

# QUANTILE REGRESSION METHOD 2 ("DOUBLY ROBUST")
# Because it runs the quantile regression on the well-matched data set,
# Our answer is not model dependent...

library(quantreg)
quantile.reg.A <- rq(re78 ~ treat + age + education + black + re74 + re75,
                     data = matched.data, 
                     tau = c(1:99/100),
                     weights = c(mout$weights, mout$weights))

# or, e.g., for just the 75th percentile effect
quantile.reg.A75 <- rq(re78 ~ treat + age + education + black + re74 + re75,
                       data = matched.data, 
                       tau = 0.75,
                       weights = c(mout$weights, mout$weights))


summary(quantile.reg.A75)                    
# $3615 is the estimated effect (double the ATE, by the way)
# Very close to the non-parametric answer

# BAD AND OVERLY SIMPLISTIC METHOD 3 TO DO QUANTILE REGRESSION 
# Just run the regression on the observational data (RAW DATA)
quantile.reg.B <- rq(re78 ~ treat + age + education + black + re74 + re75,
                     data = foo), 
tau = c(1:99/100))

# or, e.g., for just the 75th percentile effect
quantile.reg.B75 <- rq(re78 ~ treat + age + education + black + re74 + re75,
                       data = foo, 
                       tau = 0.75)

summary(quantile.reg.B75)