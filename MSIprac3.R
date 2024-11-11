# MSI Prac.03
#Q.1 1. Given 10 datapoints with mean = 2, assume that the data is Exponential(lambda)
#i.  Compute the MLE for lambda
#ii. Write a R code to generate 5000 parametric bootstrap samples each of size 10.
#iii.Compute the bootstrap means and bootstrap difference.

#ANS:
n = 10
xbar = 2

# MLE for xbar is 1/xbar
lambda_hat = 1/xbar

nboot = 5000
x = rexp(n*nboot, lambda_hat)
bootstrap_sample = matrix(x,nrow = n,ncol = nboot)
#ans end here rest is to cheak answer
head(bootstrap_sample)
length(x)
dim
dim(bootstrap_sample) # here 10x5000 means each col is of size 10 and which is the pair to be drawn

#ii)Compute the bootstrap means
# BECAUSE EACH COL HAS ONE BOOTSTRAP SAMPLE SO WE ARE FINDING THE LAMBDASTAR AS MEAN
#(here we are calculating mean)
boot_mean = apply(bootstrap_sample,2,mean) # here 2 is for col
length(boot_mean)

#iii)
# compute the bootstrap lambda_star (here we are calculating lambda)
lambda_star = 1/colMeans(bootstrap_sample)

deltastar = lambda_star-lambda_hat
length(deltastar)


#Q.2  Assume that height of a person follows Normal distribution with mean µ and standard
# deviation 10 cm. The heights of the 4 people are as follows: 168,169,173,174
# Generate 1000 parametric bootstrap samples of size 4 each.
#ANS
n = 4
sd = 10
mu = mean(c(168,169,173,174)) #MEAN ALWAYS TAKES THE ARGUMENTS AS A VECTOR
nboot = 1000
set.seed(1001)
x1 = rnorm(n*nboot,mu, sd)
length(x1)
bootstrap_sample1 = matrix(x1,nrow = n,ncol = nboot)
dim(bootstrap_sample1)
lambda_star1 = colMeans(bootstrap_sample)
deltastar1 = lambda_star1-mu
deltastar1

#Q.3According to the U.S. National Transportation Safety Board, the number of airline accidents
#by year from 1983 to 2006 were 23, 16, 21, 24, 34, 30, 28, 24, 26, 18, 23, 23, 36, 37, 49, 50,
#51, 56, 46, 41, 54, 30, 40, and 31.
#(a) For the sample data, compute the mean and its standard error and the median.
#(b) Using R, compute bootstrap estimates of the mean and median with estimates of their
#standard errors, using B = 1000 resamples.
#ANS
x = c(23, 16, 21, 24, 34, 30, 28, 24, 26, 18,23,23,36,37,49,50,51,56,46,41,54,30,40,31)

mean= mean(y)

sample_sd <- sd(y)
sample_size <- length(y)
standard_error <- sample_sd / sqrt(sample_size)

median(y)

n = 24
nboot = 1000
set.seed(1001)

install.packages("boot")
library(boot)
?boot
b = function(x,i) { #x is given sample and i is index.
  mean = mean(x[i])
  median= median(x[i])
  return(c(mean,median))
}
bs = boot(x, b, R = 1000) # bs is iterable
summary(bs)
boot.ci(bs)
bs$t # col 1 is mean of every sammple and rest is median
# bs gives the estimate of mean and median of all your sample

# Q4)
sv1=c(41.28, 45.16, 34.75, 40.76, 43.61, 39.05, 41.20, 41.02, 41.33, 40.61, 40.49, 41.77, 42.07,
      44.83, 29.12, 45.59, 41.95, 45.78, 42.89, 40.42, 49.31, 44.01, 34.87, 38.60, 39.63,
      38.52,38.52, 43.95, 49.08, 50.52, 43.85, 40.64, 45.86, 41.25, 50.35, 45.18, 39.67, 43.89,
      43.89, 42.16)
length(sv1)
library(boot)

b1 <- function(sv1,i){
  mean=mean(sv1[i])
  return(mean)
}
bs1<- boot(sv1,b1,R=1000)
bs1

summary(bs1)
# 95% CI
boot.ci(bs1)



#Q5)
sv2=c(8.26,6.33, 10.4, 5.27, 5.35 , 5.61 ,6.12 ,6.19, 5.2 ,7.01, 8.74, 7.78 ,7.02, 6, 6.5, 5.8, 5.12, 7.41
      ,6.52, 6.21, 12.28 ,5.6 ,5.38 ,6.6 ,8.74)
library(boot)

b2 <- function(sv2,i){
  cv=sd(sv2[i])/mean(sv2[i])
  return(cv)
}
bs2<- boot(sv2,b2,R=1000)
bs2

summary(bs2)
# 95% CI
boot.ci(bs2)



#Q6)
install.packages("bootstrap")
library(bootstrap)
data("law")
View(law)
head(law)
dim(law)
cor(law[,1],law[,2])

n<-15
r<-function(i,data){
  cor(data[i,1],data[i,2])
}
?jack

out<-jackknife(1:n,law,theta=r)
out
out$jack.se
out$jack.bias
out$jack.values
