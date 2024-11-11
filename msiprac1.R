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









