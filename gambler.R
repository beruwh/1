#Q.6:A reluctant gambler is dragged to the riverboat casino by his more free-willing friends. He only 
#takes $50 to gamble. Since he does not know much about gambling, he decides to play roulette. 
#At each spin, he places $25 on red. If red occurs, he wins $ 25. If black occurs, he loses his $25. 
#Therefore, the odds of winning are 50% (it’s lower in the actual game).  He will quit playing if he 
#either has zero money left or is up by $25 ($75 total). Model this process as a Markov chain. Find 
#the probability that the gambler breaks at 3rd turn. Also, examine its long-run behavior. 

library(markovchain)
library(matlab)
grmarkovchain = function(moneyMax,gain,prob = 0.5){
  ma = moneyMax/gain
  matr = zeros(ma+1)
  states = as.character(seq(0,moneyMax,by = gain))
  rownames(matr) = states ; colnames(matr) = states
  matr[1,1] = 1;matr[ma+1,ma+1] = 1
  
  for (i in 2:ma) {
    matr[i,i-1]= 1-prob;matr[i,i+1]=prob
  }
  out = new("markovchain",transitionMatrix=matr,name= paste("gambler ruin",ma+1,"dim",sep = " "))
  return(out)
}
mcGR4 = grmarkovchain(moneyMax = 75,gain = 25,prob = 0.5)
mcGR4
summary(mcGR4)

# to find solution of GR prob
mat_org = canonicForm(mcGR4)
R = mat_org[3:dim(mcGR4),1:2]
Q = mat_org[3:dim(mcGR4),3:dim(mcGR4)]
I = diag(x=1,nrow = dim(mcGR4)-2)
library(matlib) # required to find  matrix inverse
# inv(I-Q)
A = inv(I-Q)%*%R
A
length(A[,1])
ans = A[,1,drop = F] # to get first col in the maytrix
ans



after_3_step = mcGR4**3
after_3_step




#-------------------------------------------------------------------------#

#Q.7:A gambler gains Rs. 50 with probability p and loses Rs. 50 with probability q at each gamble. He 
#stops to play either when he has no money left with him or he reaches maximum amount Rs. 200. 
#Define the random variable and the corresponding probabilities. Create a DTMC in R and find the 
#probability of ruin. Find the solution when (i) p=0.3, (ii) p=0.8. 

library(markovchain)
library(matlab)
library(matlib) # required to find  matrix inverse
grmarkovchain = function(moneyMax,gain,prob = 0.5){
  ma = moneyMax/gain
  matr = zeros(ma+1)
  states = as.character(seq(0,moneyMax,by = gain))
  rownames(matr) = states ; colnames(matr) = states
  matr[1,1] = 1;matr[ma+1,ma+1] = 1
  
  for (i in 2:ma) {
    matr[i,i-1]= 1-prob;matr[i,i+1]=prob
  }
  out = new("markovchain",transitionMatrix=matr,name= paste("gambler ruin",ma+1,"dim",sep = " "))
  return(out)
}
mcGR4 = grmarkovchain(moneyMax =200 ,gain = 50,prob = 0.3)
mcGR4
summary(mcGR4)

# to find solution of GR prob
mat_org = canonicForm(mcGR4)
R = mat_org[3:dim(mcGR4),1:2]
Q = mat_org[3:dim(mcGR4),3:dim(mcGR4)]
I = diag(x=1,nrow = dim(mcGR4)-2)

# inv(I-Q)
A = inv(I-Q)%*%R
A



# to find solution of GR prob

mcGR4 = grmarkovchain(moneyMax =200 ,gain = 50,prob = 0.8)
mcGR4
summary(mcGR4)

# to find solution of GR prob
mat_org = canonicForm(mcGR4)
R = mat_org[3:dim(mcGR4),1:2]
Q = mat_org[3:dim(mcGR4),3:dim(mcGR4)]
I = diag(x=1,nrow = dim(mcGR4)-2)

# inv(I-Q)
A = inv(I-Q)%*%R
A