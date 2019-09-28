x<-runif(1000,-1,1)
y<-5+3*x+2*(x>=0)+rnorm(1000)
d <- x > 0
library(rdrobust)
rdplot(y,x)
summary(rdrobust(y,x))