# 1) A recently completed survey of subscribers to a travel magazine 
#shows that 65% of them have at least one airline credit card. 
#When compared with a similar survey taken 5 years ago, the data indicates that 40%
#of those individuals who did not have an airline credit card subsequently obtained one while 10% 
#of those who carried such cards 5 years ago no longer do so. Assuming that these trends continue into the future,
#determine the proportion of subscribers who will own airline credit cards (i) in 10 years, (ii) over the long run.

# code: 
# Xn: 
P1<-matrix(c(0.6,0.4,0.1,0.9),nrow=2,ncol=2,byrow=T)
PP1=new("markovchain",transitionMatrix=P1)
PP1
#i)
a_0_1<-c(0.35,0.65)
a_2_1<-a_0_1*PP1^2
a_2_1
#P(X2=1)=0.7625
# Comment: The proportion of subscribers who will own airline credit cards in 10 years is 0.7625
#ii)
summary(PP1)
steadyStates(PP1)  
#limiting distribution is stationary distribution
# proportion of subscribers who will own airline credit cards  over the long run is 0.8

#--------------------------------------------------------------------------------------------------------------------#

# 2) The geriatric ward of a hospital lists its patients as bedridden or ambulatory. 
#Historical data indicate that over a one-week period, 30% of all ambulatory patients are discharged, 
#40% remain ambulatory, and 30% are remanded complete bed rest. During the same period, 
#50% of the bedridden patients become ambulatory, 20% remain bedridden, and 30% die. 
#Currently the hospital has 100 patients in its geriatric ward with 30 bedridden and 70 ambulatory. 
#Determine the status of these patients over the long run. 
#The status of the discharged patient does not change if the patient dies.

# code:
#Xn=state of the patient at the end of nth week
#S={1=ambulatory,2=bedridden,3=discharged,4=dead}

m=matrix(data=c(0.4,0.3,0.3,0,0.5,0.2,0,0.3,0,0,1,0,0,0,0,1),byrow=TRUE,nrow=4)
m
P=new("markovchain",states=c("1","2","3","4"),transitionMatrix=m,name="TPM")
P
a_0_2<-c(0.7,0.3,0,0)
summary(P)
period(p)
steadyStates(P)
#The markov chain is NOT irreducible
#Hence the chain is not ergodic and thus the limiting distribution does not exist
#Thus status of the patient cannot be determined over the long run

#--------------------------------------------------------------------------------------------------------#

# Q3) An engineering professor acquires a new computer once every two years.
#The professor can choose from three models: M1, M2, M3. If the present model is M1, 
#the next computer can be M2 with probability 0.25 or M3 with probability 0.1. 
#If the present model is M2, the probabilities of switching to M1 and M3 are 0.5 and 0.15 respectively.
#And if present model is M3, then the probabilities of purchasing M1 and M2 are 0.7 and 0.2 respectively. 
#What is the probability that he will use M1, M2 and M3 in the long run?

#code:
#Xn=model chosen by the professor once in every two year
#S=(M1,M2,M3)

m=matrix(data=c(0.65,0.25,0.1,0.5,0.35,0.15,0.7,0.2,0.1),byrow=TRUE,nrow=3)
m
P=new("markovchain",states=c("M1","M2","M3"),transitionMatrix=m,name="TPM")
P
a_0_3<-c(1/3,1/3,1/3)
summary(P)
period(P)
steadyStates(P)
#The chain is irreducible and is aperiodic and hence is an ergodic chain
#Then the limiting distribution exists and is equal to the stationary distribution
# The probability he will use M1 in the long run = 0.6149584
# The probability he will use M2 in the long run = 0.2714681 
# #The probability he will use M3 in the long run = 0.1135734

#------------------------------------------------------------------------------------------------------------#

# Q4) The manufacturer of Hi-Glo toothpaste currently controls 60% of the market in a particular city.
#Data from previous year shows that 88% of Hi-Glo’s customers remained loyal to Hi-Glo, 
#while 12% Hi-Glo customers switched to rival brands. In addition,
#85% of the competition’s customers remained loyal to the competition. 
#What will be the market share of the Hi-Glo after (i) 5 years, (ii) 10 years?

# code:
#Xn=brand demand in the market at the nth year
#S={1,2}
#1=Hi-Glo , 2=rival competition

m=matrix(data=c(0.88,0.12,0.15,0.85),byrow=TRUE,nrow=2)
m
P=new("markovchain",states=c("1","2"),transitionMatrix=m,name="TPM")
P

#Hi-Glo toothpaste currently controls 60% of the market in a particular city.
#P[X0=1]=0.6
#P[X0=2]=0.4

P_0=c(0.6,0.4)

#i)the market share of the Hi-Glo after 5 years
P_5=P_0*(P^5)
P_5
# the market share of the Hi-Glo after 5 years is 0.5647692

#ii)the market share of the Hi-Glo after 10 years
P_10=P_0*(P^10)
P_10
# the market share of the Hi-Glo after 10 years is 0.5574656

#-------------------------------------------------------------------------------------------------------------#

# Q5) The training program for production supervisors at a particular company consists of two phases.
#Phase 1 is 3-week classroom work and phase 2 is a 3-week apprenticeship program. From experience, 
#the company expects only 60% of those beginning classroom training to graduate into the apprenticeship phase,
#with the remaining 40% dropped completely from the training program. Of those who completed the
#apprenticeship phase, 70% graduated as supervisors, 10% were asked to repeat the second phase and 20%
#dropped completely from the program. Find the limiting distribution of the chain. What % of employees
#will drop out from the training program? What % of employees will complete the training program?

# code:
#State 1 (Classroom Training): The initial phase where employees start.
#State 2 (Apprenticeship Training): The second phase for those who pass the first.
#State 3 (Dropout): The state for those who drop out of the program.
#State 4 (Graduated): The state for those who complete the program.
#S={1,2,3,4}

m=matrix(c(0, 0.6, 0.4, 0, 0, 0.1, 0.2, 0.7, 0, 0, 1, 0, 0, 0, 0,1),
         nrow = 4, byrow = TRUE)
m
P=new("markovchain",states=c("1","2","3","4"),transitionMatrix=m,name="TPM")
P

summary(P)
#The Markov Chain is NOT irreducible
#Here the given markov chain is not ergodic chain,
#hence limiting distribution is not equal to stationary distribution

#initial_state <- c(1, 0, 0, 0)

in_state=c(1, 0, 0, 0)

limit=in_state *(P^100)
limit

dropout_percentage =limit[3] * 100
dropout_percentage
graduated_percentage =limit[4] * 100
graduated_percentage

# percentage of employees will drop out from the training program is 53.33333
# percentage of employees will complete the training program is 46.66667
