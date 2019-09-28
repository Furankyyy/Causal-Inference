# If you are copy/pasting this to an R Notebook, select "Raw" from the GitHub page before you copy.

# Replication file of Section 5 in
# Iacus, King, Porro (2011), Multivariate Matching Methods 
# That Are Monotonic Imbalance Bounding, JASA, V 106, N. 493,
# p. 345-361

foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))

# Table 1 in Ebonya
# Indep. vars
# anygirls: Any female children
# ngirls: Number of female children
# totchi: Total number of children

# white: White=1, 0 otherwise
# female: Female=1, 0 otherwise
# age: age
# srvlng: Service length (years)

# reg1 - reg9 are regional binary variables
# binary religious variables: none, Protestant, Catholic, OtherC, OtherR
# binary party variables: Dems, Repubs, OthParty
# DEPENDENT VARIABLE: nowtot

#### BREAKOUT 1 ####
## Perform genetic matching with the following variables:
## Dems, Repubs, Christian, age, srvlng, demvote
## Tr  = hasgirls
## Set pop.size = 20, set nboots = 200... complete the instructions below...

set.seed(2324); 
genout <- GenMatch(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), pop.size = 20, nboots = 200)

mout <- Match(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = genout)

MatchBalance( foo$hasgirls ~ foo$Dems + foo$Repubs + foo$Christian + foo$age + foo$srvlng + foo$demvote, match.out = mout)

## If you obtain really high balance, consider rerunning with M = 2 or 3...
## If/when you are satisfied by balance achieved, then rerun Match() with Y included 
## and obtain the treatment effect estimate, the standard error, and the confidence interval.

mout <- Match(Y = foo$nowtot, Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = genout)


summary(mout)

## If you get to the end and have extra time, you can try changing X (remember to change
## it in GenMatch, in Match, and also in the formula for MatchBalance().
## For example, you could try to control more finely for religion. You could try to control
## for race (e.g., binary "white" variable), or for region (reg1, reg2, etc.) 

#### Step 2 ####
## Re-run genetic matching using a different causal question
## What's the impact of having two girls and no boys vs. 2 boys and no girls?

## Same data set as before
foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))

## Here is some code to get you started...
which.two.or.more.girls <- which(foo$ngirls >= 2 &  foo$nboys == 0)
which.two.or.more.boys <- which(foo$ngirls == 0 & foo$nboys >= 2)

subset2kids <- foo[c(which.two.or.more.girls, which.two.or.more.boys), ]

## Perform genetic matching with the following variables:
## Dems, Repubs, Christian, age, srvlng, demvote
## Tr  = hasgirls
## Set pop.size = 100, set nboots = 250... complete the instructions below...

set.seed(2324)

genout2 <- GenMatch(Tr = subset2kids$hasgirls, X = cbind(subset2kids$Dems,subset2kids$Repubs,
                                                                         subset2kids$Christian,subset2kids$age,
                                                                         subset2kids$srvlng,subset2kids$demvote), pop.size = 20, nboots = 200)

mout2 <- Match(Tr = subset2kids$hasgirls, X = cbind(subset2kids$Dems,subset2kids$Repubs,
                                                    subset2kids$Christian,subset2kids$age,
                                                    subset2kids$srvlng,subset2kids$demvote), Weight.matrix = genout2)

MatchBalance(subset2kids$hasgirls ~ subset2kids$Dems+subset2kids$Repubs+
             subset2kids$Christian+subset2kids$age+
             subset2kids$srvlng+subset2kids$demvote, match.out = mout2)

## If you obtain really high balance, consider rerunning with M = 2 or 3...
## If/when you are satisfied by balance achieved, then rerun Match() with Y included 
## and obtain the treatment effect estimate, the standard error, and the confidence interval.

mout2 <- Match(Y=subset2kids$nowtot ,Tr = subset2kids$hasgirls, X = cbind(subset2kids$Dems,subset2kids$Repubs,
                                                    subset2kids$Christian,subset2kids$age,
                                                    subset2kids$srvlng,subset2kids$demvote), Weight.matrix = genout2)


summary(mout2)

## IMPORTANT: SAVE YOUR mout2 object!
## Follow this protocol: save(object_name, file = "object_filename") to save to working directory
## e.g., if you have an object named mmm, save(mmm, file = "mmm")
## To retrieve your object, you would type: load
## If you get to the end and have extra time, you can try changing X (remember to change
## it in GenMatch, in Match, and also in the formula for MatchBalance().
## For example, you could try to control more finely for religion. You could try to control
## for race (e.g., binary "white" variable), or for region (reg1, reg2, etc.) 
