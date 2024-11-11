#            MSI Practical 
#Q.1 Draw random samples of size 20 and 500 from binomial distribution with parameters p =  0.5, n = 10.  
#a. Plot rod plots for empirical pmf and theoretical pmf. (Two different plot)
#b. Plot empirical and theoretical cdfs. (in one graph.) 
#c. Comment on the effect of increase in sample size. 
#d. Estimate the parameters by method of maximum likelihood. 
#Ans:

#i)

par(mfrow = c(1,2))

n <- 10
p <- 0.5
x <- 0:20

#theo_pmf 
pmf <- dbinom(x,n,p)
plot(x,pmf,type = "l",main = "Theoretical pmf")

#emp_pmf
install.packages("prevtoinc")
library(prevtoinc)

set.seed(404)

rs <- rbinom(20,10,0.5)
emp_pmf <- epmf(rs)
plot(emp_pmf,type = "l",main = "Empherical pmf")

#now for 500 rs

par(mfrow = c(1,2))

n <- 10
p <- 0.5
x <- 0:500

#theo_pmf 
pmf <- dbinom(x,n,p)
plot(x,pmf,type = "l",main = "Theoretical pmf")

#emp_pmf
install.packages("prevtoinc")
library(prevtoinc)

set.seed(5525)

rs <- rbinom(500,10,0.5)
emp_pmf <- epmf(rs)
plot(emp_pmf,type = "l",main = "Empherical pmf")

#ii)

par(mfrow = c(1,2))

n <- 10
p <- 0.5
x <- 0:20

#theoretical cdf
cdf <- pbinom(x,n,p)
#Empherical cdf
emp_cdf <- ecdf(rs)
plot(x,cdf,type = "l",main = "Theoretical cdf")
lines(emp_cdf,col = "red")

#for 500

n <- 10
p <- 0.5
x <- 0:500

#theoretical cdf
cdf <- pbinom(x,n,p)
#Empherical cdf
emp_cdf <- ecdf(rs)
plot(x,cdf,type = "l",main = "Theoretical cdf")
lines(emp_cdf,col = "red")

#iii) comment: As the sample size increases the theoretical pmf and cdf tends to empherical pmf and cdf

#iv)

#for 20

n <- 10
p <- 0.5
x <- 20

rs <- rbinom(x,n,p)

logl <- function(p){
  -sum(dbinom(rs,n,p,log = T))
}

optim(fn = logl, par = c(p = 0.1),method = "BFGS")

#for 500

n <- 10
p <- 0.5
x <- 500

rs <- rbinom(x,n,p)

logl <- function(p){
  -sum(dbinom(rs,n,p,log = T))
}

optim(fn = logl, par = c(p = 0.1),method = "BFGS")

#-----------------------------------------------------------------------------#


#Q.2 : X ~ Poi (λ= 1.5). 
#  Draw random samples of sizes 10, 50, 100 from the distribution of X and estimate the 
#  parameter by method of maximum likelihood. Comment on the results
#ANS: 

#for 10
lambda <- 1.5
x <- 10
rs <- rpois(x,lambda)
logl <- function(lambda)
{
  -sum(dpois(rs,lambda = 0.5,log = T))
}

optim(fn = logl,par = c(lambda = 0.5),method = "BFGS")

#for 50
lambda <- 1.5
x <- 50
rs <- rpois(x,lambda)
logl <- function(lambda)
{
  -sum(dpois(rs,lambda = 0.5,log = T))
}

optim(fn = logl,par = c(lambda = 0.5),method = "BFGS")

#for 100
lambda <- 1.5
x <- 100
rs <- rpois(x,lambda)
logl <- function(lambda)
{
  -sum(dpois(rs,lambda = 0.5,log = T))
}

optim(fn = logl,par = c(lambda = 0.5),method = "BFGS")

# Comment: As the sample size increases, MLE becomes approximately equal to lambda

#------------------------------------------------------------------#

# Q.3. Draw random samples of size 20 and 100 from N (5,2) distribution. Estimate the parameters 
#      by method of MLE. Comment on the effect of increase in sample size.
#ANS:

set.seed(1001)

n <- 20
mu <- 5
var <- 2
sd <- sqrt(var)

theta <- c(mu,sd)

#generate random sample
rs <- rnorm(n,theta[1],theta[2]);rs

#mle
logl <- function(theta){
  -sum(dnorm(rs,mean = theta[1],sd = theta[2],log = T))
}

theta_ini = c(2,1)
optim(fn = logl,par = theta_ini)

#comment: increasing sample size generally improves accuracy of parameter estimates.

#------------------------------------------------------------------------------------------------------#

#Q4 . The number of particles per square meter is given below:
#11, 11, 11, 10, 8, 11, 9, 10, 5, 11, 13, 16, 10, 9, 12, 12, 8, 9, 10, 14, 10, 7, 2, 11, 13, 16, 10, 11, 
#10, 3, 5, 11, 5, 12, 7
#a. Do you think the Poisson model is appropriate? If yes, estimate the parameter by 
#method of maximum likelihood.
#b. Using the parameter estimate obtained in a. above, generate random samples of sizes 
#100 and 150 and estimate the parameters.
#c. Comment on the results.
#ANS: 
library(stats4)
library(methods)
library(prevtoinc) 
# a)
set.seed(100)
x<-c(11,11,11,10,8,11,9,10,5,11,13,16,10,9,12,12,8,9,10,14,10,7,2,11,13,16,10,11,10,3,5,11,5,12,7)
x
mu<-mean(x)
mu
var<-var(x)
var
nll.pois <- function(lambda)
{
  -sum(dpois(x,lambda=lambda,log=TRUE))
}
optim(fn=nll.pois,par=c(lambda=0.5),method="BFGS")

# b)
# for size 100
set.seed(101)
N <- 100
x<-rpois(N,9.8)
x
nll.pois <- function(lambda)
{
  -sum(dpois(x,lambda=lambda,log=TRUE))
}
optim(nll.pois,par=c(lambda=0.5),method="BFGS")

# for size 150
#set.seed(101)
N<-150
x<-rpois(N,9.8)
x
nll.pois<-function(lambda)
{
  -sum(dpois(x,lambda = lambda,log=TRUE))
}
optim(nll.pois,par=c(lambda=0.5),method="BFGS")
# c)
# comment: As the sample size increases, MLE becomes approximately equal to lambda


#Q5 Generate samples of sizes 50, 100 and 120 from the following distributions and estimate the 
#parameters by method of maximum likelihood (set seed as 100):
# i. Exponential distribution (rate =2)
#ii. Gamma distribution (shape=2, scale=1)
#Comment on the effect of increase in sample size
#ANS:

#i)
library(stats4)
library(methods)
library(prevtoinc)
set.seed(100)
x1 <- rexp(50,2)
x2 <- rexp(100,2)
x3 <- rexp(120,2)

logl <- function(rate){
  -sum(dexp(x1,rate = rate,log = T))
}
optim(fn = logl,par = c(rate = 0.2),method = "BFGS")


#for 100

logl <- function(rate){
  -sum(dexp(x2,rate = rate,log = T))
}
optim(fn = logl,par = c(rate = 0.2),method = "BFGS")

#for 120

logl <- function(rate){
  -sum(dexp(x3,rate = rate,log = T))
}
optim(fn = logl,par = c(rate = 0.2),method = "BFGS")

#ii)

n1 = 50
set.seed(100)
shape = 2
scale = 1
vec = c(shape,scale)
x1 = rgamma(n1, shape =2 ,scale =  1);x1
logl = function(vec)
{
  -sum(dgamma(x1,shape = vec[1],scale = vec[2] ,log = T))
}  
ini_vec = c(shape = 0.5,scale = 0.1)
optim(fn = logl,par = ini_vec)

n1 = 100
set.seed(100)
shape = 2
scale = 1
vec = c(shape,scale)
x1 = rgamma(n1, shape =2 ,scale =  1);x1
logl = function(vec)
{
  -sum(dgamma(x1,shape = vec[1],scale = vec[2] ,log = T))
}  
ini_vec = c(shape = 0.5,scale = 0.1)
optim(fn = logl,par = ini_vec)

n1 = 120
set.seed(100)
shape = 2
scale = 1
vec = c(shape,scale)
x1 = rgamma(n1, shape =2 ,scale =  1);x1
logl = function(vec)
{
  -sum(dgamma(x1,shape = vec[1],scale = vec[2] ,log = T))
}  
ini_vec = c(shape = 0.5,scale = 0.1)
optim(fn = logl,par = ini_vec)

#comment: As the sample size increases, MLE of shape and scale becomes approximately
#equal to true value of shape and scale

'''





############





'''

# Q.1
# ANS :
y = c(125,18,20,34)
logl <- function(p){
  -(y[1]*log(2+p)+(y[2]+y[3])*log(1-p)+y[4]*log(p))
}
optim(fn = logl,par = c(0.5),method = "BFGS")

#ii)
p0 = 0.5
estep = function(y,p0){
  tep = c(2*y[1]/(2+p0),y[1]-2*y[1]/(2+p0),y[2],y[3],y[4])
  return(tep)
} # y11 and y12 are given in que
estep(y,p0)

emconvergence <- function(y,p0){
  pold <- p0
  pnew <- p0+0.5
  while(abs(pnew-pold)>0.0000000001){
    pold <- p0
    x <- estep(y,p0) # E-Step
    pnew <- (p0*y[1]/(2+p0)+y[4])/(p0*y[1]/(2+p0)+y[2]+y[3]+y[4]) # this is mle of complete data vector given in que 
    # M-Step
    p0 <- pnew
    
  }
  return(pnew)
}
p.em <- emconvergence(y,p0)
p.em

#------------------------------------------------------------------------------------------#

#Q 4 Load the data ‘faithful’ in R. This data consists of waiting time before eruptions.
#a. Plot a histogram and comment on the distribution of waiting times.
#b. Use EM algorithm to estimate the parameters.

#ANS
# install.packages("mixtools")
library(mixtools)
data("faithful")
head(faithful)
x = faithful$waiting
hist(x,xlab="waiting times",ylab = "frequency")
#comment on above distribution
gm = normalmixEM(x,lambda = 0.5,mu = c(20,50),sigma = 10,k = 2) # can put any value of mu 
#lambda = 0.5: This is the initial guess for the mixing proportion 𝜆.
#λ of each Gaussian component.
#there are 2peeks so 2 gaussian component, Hence k = 2
gm$lambda
gm$mu 
gm$sigma

#----------------------------------------------------------------------------------------#

#Q.5 . Load the dataset deaths in the package MASS. Implement the EM algorithm on the dataset 
#log(deaths)

#ANS
#install.packages("MASS")
library(MASS)
data("deaths")
head(deaths)
x = log(deaths)
var(x) # to find the variance of the data 
hist(x)

gm = normalmixEM(x,lambda = 0.5,mu = c(7.4,7.9),sigma = 0.81,k = 2)
gm$lambda
gm$mu 
gm$sigma

'''





############





'''

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





