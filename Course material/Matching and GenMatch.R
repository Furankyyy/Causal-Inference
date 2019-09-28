#PEACEKEEPING WORKOUT (based on  King, Gary;Zeng, Langche, 2007, 
                      #"Replication data for: When Can History be Our Guide? 
                      #The Pitfalls of Counterfactual Inference", 
                     # https://hdl.handle.net/1902.1/DXRXCFAWPK, 
                     # Harvard Dataverse, V4, 
                     # UNF:3:DaYlT6QSX9r0D50ye+tXpA== [fileUNF] )
# CONSIDER USING THE JUPYTER NOTEBOOK WITH R-SERVER KERNEL (NEVER R-SAGE KERNEL)
library(Matching)
library(rgenoud)
library(MASS)

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo); Y = foo$pbs2s3 # define Y for the fun of it, if you want to use Y later...
### Outcome is "pbs2s3": "democracy" and "peace" w/in 2 years after the end of the war
### codebook is here: http://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf

### Treatment indicator is "untype4": "multidimensional peacekeeping/peacebuilding"

# STEP 1: Propensity score modeling # e.g, 
glm1 <- glm(untype4 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, family = binomial, data = foo)

# number of treated units:
sum(foo$untype4)

# number of controls:
length(foo$untype4) - sum(foo$untype4)

mout_propensityscore <- Match(Tr=foo$untype4, X=glm1$fitted)

mout_multivariate <- Match(Tr=foo$untype4, X = cbind(foo[,c(11:16, 18:21)]))

# PROPENSITY SCORE BALANCE RESULTS
mb1  <- MatchBalance(untype4 ~  wartype + logcost + wardur + factnum + 
                      factnum2 + trnsfcap + 
                      treaty + develop + exp + decade, 
                    data=foo, match.out = mout_propensityscore, nboots=500)
# MULTIVARIATE MATCHING BALANCE RESULTS
mb2  <- MatchBalance(untype4 ~  wartype + logcost + wardur + factnum + 
                      factnum2 + trnsfcap + 
                      treaty + develop + exp + decade, 
                    data=foo, match.out = mout_multivariate, nboots=500)



# STEP 2:
# Perform genetic matching, with X = to all the variables included in MatchBalance
# pop.size should be at least 200, max.generations should be at least 20,
# and wait generations should be at least 10. Estimand is "ATT".
X <- cbind(foo$wartype,foo$logcost,foo$wardur,foo$factnum, 
           foo$factnum2,foo$trnsfcap,foo$treaty,foo$develop,foo$exp,foo$decade)

gen <- GenMatch(Tr=foo$untype4,X=X,pop.size=500,max.generation=50,wait.generations=10)

# Take note of the same 3 results as above. Check the rgenoud output
# to see if the genetic algorithm improves fitness over time.


#The results of GenMatch is the best weights it identifies.
#To do the actual matching, use Match() again with the weights in GenMatch()
mout <- Match(Tr=foo$untype4,X=X,M=1,Weight.matrix = gen)

mb.gen <- MatchBalance(untype4~wartype + logcost + wardur + factnum + 
                         factnum2 + trnsfcap + 
                         treaty + develop + exp + decade, 
                       data=foo, match.out = mout, nboots=500)
# STEP 3:
# Augment your STEP 2 analysis by including your propensity score in "X"...
# e.g., X <- cbind(X, glm1$fitted)
# Take note of the same 3 results. See if your balance improves with inclusion
# of the propensity score as a matching variable.  See if your impact
# estimate changes. See if you can improve your results (e.g., balance)
