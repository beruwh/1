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
var(x) # to find the varience of the data 
hist(x)

gm = normalmixEM(x,lambda = 0.5,mu = c(7.4,7.9),sigma = 0.81,k = 2)
gm$lambda
gm$mu 
gm$sigma
