set.seed(20181001)
### LOOCV 

# Load packages and data
library(Matching)
library(boot)
data(lalonde)

# Train your model on ALL the data -- Use glm instead of lm
glm.lalonde <- glm(re78 ~ ., data = lalonde)
glm2.lalonde <- glm(re78 ~ .*., data = lalonde)  # all predictors with all first-order interaction terms

# cv.glm performs LOOCV (You can use the K argument to perform K-fold CV)
cv.err <- cv.glm(lalonde, glm.lalonde)
cv.err2 <- cv.glm(lalonde, glm2.lalonde)

# This is the MSE
cv.err$delta[1]  
cv.err2$delta[1]  # The second model overfits the data and has a larger MSE

### BOOTSTRAPPING

# Declare the auxiliary function
boot.fn <- function(data, index) return(coef(lm(re78 ~ ., data = data, subset = index)))

boot.fn(lalonde, 1:nrow(lalonde)) # Check whether the function is working as expected
boot.lalonde <- boot(lalonde, boot.fn, 10000)

# Retrieve standard errors
boot.se <- apply(boot.lalonde$t, 2, sd)
lm.se <- summary(lm(re78 ~ ., data = lalonde))$coef[,2]
format(data.frame(boot.se, lm.se), digits=2, scientific=F)
conf.intervals <- apply(boot.lalonde$t, 2, quantile, c(0.025, 0.975))