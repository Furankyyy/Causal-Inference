#EXERCISE TO BUILD INTUITION FOR CORRELATED VS. UNCORRELATED DATA
# PLEASE FOCUS ON UNDERSTANDING THE BELOW
### DO NOT JUST EXECUTE ALL THE CODE IN ONE BATCH--RUN IT LINE BY LINE...

### Simulation of analysis on correlated data

set.seed(1314)

nsims <- 10000

storage_correlated_df <- matrix(NA, ncol = 10000, nrow = 100)

# GO THROUGH THE BELOW, LINE BY LINE, & TRY TO UNDERSTAND EVERYTHING
# FOCUS ON WHATEVER YOU FIND CONFUSING

set.seed(2321)
for(j in 1:nsims) {
  
  simulated_original_data <- rnorm(100)
  
  for(i in 1:100)
  {
    
    a_bootstrapped_sample <- sample(simulated_original_data, 100, replace = TRUE)
    summary_statistic_of_boot_sample <- mean(a_bootstrapped_sample)
    storage_correlated_df[i,j] <- summary_statistic_of_boot_sample
  }
  
}

## ANALYSIS WITH THE RESULTS...
correlated_means <- apply(storage_correlated_df, 2, mean)
print(mean(correlated_means))

var_of_correlated_means <- var(correlated_means)
print(var_of_correlated_means)

### NOW, what if our data is not correlated?

storage.NOTcorr.df <- matrix(NA, ncol = 10000, nrow = 100)

# AGAIN, GO THROUGH THE BELOW, LINE BY LINE, & TRY TO UNDERSTAND EVERYTHING
# FOCUS ON WHATEVER YOU FIND CONFUSING

set.seed(2353)
for(j in 1:nsims) {
  for(i in 1:100)
  {
    an_independent_sample_of_the_data <- rnorm(100)
    storage.NOTcorr.df[i,j] <- mean(an_independent_sample_of_the_data)
  }
  
}

## ANALYSIS WITH THE RESULTS...
NOTcorr_means <- apply(storage.NOTcorr.df, 2, mean)
print(mean(NOTcorr_means))
var_of_NOTcorr_means <- var(NOTcorr_means)
print(var_of_NOTcorr_means)

## SYNTHESIZING THE RESULTS (is the var of uncorrelated smaller, and if so, by how much?)
percent_var_redux <- 100*(var_of_correlated_means - var_of_NOTcorr_means)/var_of_correlated_means
print(percent_var_redux)

# LET'S VISUALIZE!!!
par(mfrow = c(4,1))
hist(correlated_means, main = "Distribution of Correlated Means")
## WHEN YOU RUN THE HIST BELOW, COMPARE THE SCALE OF THE X-AXIS TO THE HIST ABOVE
hist(NOTcorr_means, main = "Distribution of NOT Correlated Means")

# HERE IS THE SAME INFO IN ONE FIGURE, WITH LINES SHOWING DENSITY...
plot(density(NOTcorr_means), col = "blue", lwd = 2)
lines(density(correlated_means), col = "red", lwd = 2) # 

# IF YOU HAVE TROUBLE INTERPRETING THIS, HERE IT IS ON A DIFFERENT SCALE...
plot(density(correlated_means), col = "red", lwd = 2)
lines(density(NOTcorr_means), col = "blue", lwd = 2)











################ PRELIMINARIES
library(MASS)
data(Pima.tr)
library(tree)
library(randomForest)

## STEP 1: Logistic regression ##
logistic_reg <- glm(type ~ ., data = Pima.tr, family = binomial) # basic model
predict_logistic.tr <- predict(logistic_reg, type = "response")  # predicted probabilities (TRAINING SET)

# Create a function that evaluates the misclassification rate for TRAINING SET, for any threshold
evaluate_fn <- function(threshold = NA)
{
  predicted_outcomes <- as.numeric(predict_logistic.tr > threshold)
  table_logistic <- table(Pima.tr$type, predicted_outcomes)
  
  error_rate_logistic <- sum(table_logistic[2:3])/sum(table_logistic)
  return(error_rate_logistic)
}

# Optimize for threshold within TRAINING SET
best_threshold <- optim(0.5, evaluate_fn)$par

# Produce predicted probabilities for the test set
predict_logistic <- predict(logistic_reg, newdata = Pima.te, type = "response")

# Convert those predicted probabilities to predicted TEST SET outcomes
predicted_logistic_outcomes <- as.numeric(predict_logistic > best_threshold)

# Measure misclassification error, in TEST SET
table(Pima.te$type, predicted_logistic_outcomes)
table_logistic <- table(Pima.te$type, predicted_logistic_outcomes)

error_rate_logistic <- sum(table_logistic[2:3])/sum(table_logistic)
print(error_rate_logistic)

## Basic tree ##
basic_tree <- tree(type ~., data = Pima.tr)
predict_basic_tree <- predict(basic_tree, newdata = Pima.te, type = "class")

table(Pima.te$type, predict_basic_tree)
table_basic_tree <- table(Pima.te$type, predict_basic_tree)

error_rate_basic_tree <- sum(table_basic_tree[2:3])/sum(table_basic_tree)
print(error_rate_basic_tree)

## Pruned tree ##
pruned_tree <- cv.tree(basic_tree,FUN=prune.misclass)
print(pruned_tree)

pruned_tree <- prune.misclass(basic_tree, k = pruned_tree$k[which.min(pruned_tree$dev)])
predict_pruned_tree <- predict(pruned_tree, newdata = Pima.te, type = "class")

#NEXT STEP: Evaluate pruned tree performance in the test set
table(Pima.te$type,predict_pruned_tree)
table_pruned_tree <- table(Pima.te$type,predict_pruned_tree)

error_rate_pruned_tree <- sum(table_pruned_tree[2:3])/sum(table_pruned_tree)
print(error_rate_pruned_tree)

## Random forest ##

#STEP 1: Run Random Forest model
forest <- randomForest(type???.,data=Pima.tr, mtry=3,importance =TRUE)
predict_forest <- predict(forest,newdata = Pima.te, type="class")

#STEP 2: Use your model to predict for the test set

#STEP 3: Evaluate model performance in the test set