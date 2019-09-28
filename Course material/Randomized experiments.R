storage.vector <- NA

# Function that assigns treatment/control depending on 
# propensity scores (assignment probabilities)
experiment <- function(vector.of.probabilities = NULL) {
  k = 1
  for (i in 1:length(vector.of.probabilities)) {
    if(
      sample(x = c(1,0), size = 1, prob = c(vector.of.probabilities[i], 
                                            1 - vector.of.probabilities[i])) == 1) {
      storage.vector[k] <- i
      k = k + 1
    }
  }
  return(list(treated.units = storage.vector, 
              control.units = (1:(length(vector.of.probabilities)))[-storage.vector]))
}

### Here are two distributions
# Incomes for the female-headed households without children are defined per the following code:
set.seed(123); nokids.income <- round(abs(exp(rnorm(1000, 5, 1))))

# Household sizes for the female-headed households with children are defined per this code:
set.seed(123); kids.hhsize <- round(sqrt(abs(rnorm(1000, 12, 100))) + .3)




###Randomized experiment

# Instructions

city.names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J") 



observed.turnout = c(17, 30, 13, 55, 26, 29, 48, 43, 17, 30)
observed.diffmeans <- mean(observed.turnout[c(2,4,6,8,10)]) - 
  mean(observed.turnout[c(1,3,5,7,9)])

print(observed.diffmeans)

foo <- data.frame(city.names, observed.turnout)

# Turnout Function
turnout <- function() {
  # Four coin flips, establishing random assignment
  assignment        <- foo[sample(1:2),]
  assignment[3:4,]  <- foo[sample(3:4),]
  assignment[5:6,]  <- foo[sample(5:6),]
  assignment[7:8,]  <- foo[sample(7:8),]
  assignment[9:10,] <- foo[sample(7:8),]
  
  treatment.group   <- assignment[c(1,3,5,7,9),]
  control.group     <- assignment[c(2,4,6,8,10),]
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Turnout Function
iter.RI <- function(iterations = 10000) {
  for (i in 1:iterations) 
  {storage.vector[i] <- turnout()
  }
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()

# Exploring the results
vvv <- quantile(results, prob = c(0.95))
length(unique(results))
hist(results)
plot(density(results))
abline(v = vvv, lwd = 2, col = "red")
