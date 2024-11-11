#Q1
 
install.packages("markovchain")

library(markovchain)

p <- matrix(data = c(0.4,0.6,0.2,0.8),byrow = T,
            nrow = 2);p

pp <- new("markovchain", transitionMatrix = p);pp

#here we need to change the name of the markov chain
#as well as the state space

pp <-  new("markovchain",states = c("Defective Item","Good Item"),
           transitionMatrix = p,name = "Practical 2 Q.1 - Production Process");pp

# to see the TPM in the transitional form
plot(pp)

#comment : The probability that the first defectiveitem to appear is the 
# the fifth item is 0.1024

#------------------------------------------------------------------------------------#

#Q2

m <- matrix(data = c(0.3,0.2,0.5,0.5,0.1,0.4,0.5,0.2,0.3),byrow = T,nrow = 3);m
p <- new("markovchain",states = c("0","1","2"),transitionMatrix = m,name = "DTMC");p

#i)

p_0 <- c(0.5,0.5,0) #This vector represents the initial probabilities 
#for each state: 50% chance of starting in state 0, 
#50% chance in state 1, and 0% chance in state 2.

ans_1 <- p_0[2]*p["1","1"]*p["1","0"];ans_1

plot(p)

#ii)

p_1 <- p_0*p;p_1

ans_2 <- p_1[3]*p["2","0"]*p["0","0"];ans_2

p_3 <- p_0*p^3;p_3

#i) P(X0 = 1,X1 = 1,X2 = 0) = 0.025
#ii) P(X1 = 2,X2 = 0,X3 = 0) = 0.0675
#iii) Prob dist of X3 is {0.416,0.1815,0.4025}

#Q3

#lets assume states are 0,1,2 hence, s = {0,1,2}
m <- matrix(data = c(0,1,0,1/6,1/2,1/3,0,2/3,1/3),byrow = T,nrow = 3);m

p <- new("markovchain",states = c("0","1","2"),transitionMatrix = m,name = "TPM");p

#i)

plot(p)

#ii)

p_0 <- c(1/3,2/3,0);p_0

p_3 <- p_0*p^3;p_3

#comment: The marginal dist is {0.09876543,......,......}

#Q4

# here the state space is S = {0,1}

m <- matrix(data = c(0.6,0.4,0.1,0.9),byrow = T,nrow = 2);m

p <-  new("markovchain",states = c("0","1"),transitionMatrix = m,name = "TPM");p

#i)

# proportions of subscribers who will own the card in the 10 years
#p[xn+2 = 1] = p11_(2)
a_0 <- c(0.35,0.65);a_0

p11_2 <- a_0*p^2;p11_2
ans <- p11_2[1,2];ans

#ii)

summary(p)

steadyStates(p)

#Q5

m <- matrix(c(0.5,0.3,0.2,0.5,0.2,0.3,0.4,0.5,0.1),byrow = T,nrow=3);m

p <- new("markovchain",transitionMatrix = m,states = c("Sunny","cloudy","Rainy"),
         name = "Weather");p

plot(p)

summary(p)

#i) calculate the prob that the day after tommorow is rainy given that today is rainy

c1 <-  p**2;c1

ans1 <- c1[3,3];ans1

#ii) calculate the prob that after 4 days it will be sunny given that today is cloudy

c2 <- p**4;c2

ans2 <- c2[2,1];ans2

#iii) Calculate the prob that the day after tommorow is rainy starting from an
#equal initial distibution 

a0 <- c(1/3,1/3,1/3);a0

a2 <- a0*c1;a2

ans3 <- a2[3];ans3

#iv) calculate the prob that after 4 days it will be sunny starting from
#an equal initial dist

b <- a0 * c1;b

ans4 <- b[1];ans4



























