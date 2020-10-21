
#Set parameters
G=500; 
N =10000 #total pop size
c = 0.1 #cost of resistance
comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
muvalues<-1/N*c(0.001,0.01,0.1,1,10)
n=100
for (mu in muvalues[1:3]){
  #mu = muvalues[2]
  print(paste0("mu ", mu, " NoWT_YesCR"))
  for (j in 1:10){#starting the sim and  plotting it! 
    #j = 1
    
    #Read in existing simulation data
    popArrayDF = read.csv(
      file = paste0("SimData/NoWT_YesCR_popArrayDF",G,"Gen",comp,"comp",mu,"mu", j, "j.csv"),
      header = TRUE)
    popArrayDF<-popArrayDF[,2:(ncol(popArrayDF)-1)] #Remove some unnecessary columns
    
    #Calculate pop fitness for every generation
    compindeces=3:(ncol(popArrayDF)-1)
    rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
    popfitness = c()
    for (i in 1:nrow(popArrayDF)){
      fitnessArray=popArrayDF[i,]*rawfitnessarray
      popfitness = c(popfitness, sum(fitnessArray[1,])/N)    
    }
    
    #Calculate probability of WT mutant escape | no escape has happened yet
    #This would be mu times the num resistant bacteria times 2 times s_b (fitness benefit of the WT compared to pop)
    ProbRaw = mu* popArrayDF$Res* 2* ((1/popfitness)-1)
    #Calculate real probability  (taking into account that it may have happened before)
    ProbReal = c()
    ProbNo = c()
    i=1
    ProbReal = c(ProbReal, ProbRaw[i]*1)
    ProbNo = c(ProbNo, 1-ProbReal)
    for (i in 2:nrow(popArrayDF)){
      ProbReal = c(ProbReal, ProbNo[i-1]*ProbRaw[i])
      ProbNo = c(ProbNo, ProbNo[i-1]-ProbReal[i])
    }
    
    plot(ProbRaw, ylim=c(0,1), main = paste0("mu ", mu, " j ", j), type="l")
    #points(ProbReal, col=2, type="l")
    points(1-ProbNo,col="purple", type="l")
    points(popArrayDF$WT/N, type="l", col="red", lwd=2)
    points(popArrayDF$CR/N, type="l", col="blue", lwd=2)
    points(popArrayDF$Res/N, type="l", col="orange", lwd=2)
    points(1-popArrayDF$Res/N-popArrayDF$CR/N-popArrayDF$WT/N, type="l", col="green", lwd=3)
    
    
    #Prob WT Escape total
    ProbWTEscape=max(1-ProbNo)
    print(ProbWTEscape)
  }
}

#####DETERMINISTIC APPROX
s_comp = (1-(c*(1-comp)))/(1-c) - 1

for (mu in muvalues[2]){
  #mu = muvalues[2]
  print(paste0("mu ", mu, " NoWT_YesCR"))
  for (j in 1:10){#starting the sim and  plotting it! 
    #j = 1
    #Read in existing simulation data
    popArrayDF = read.csv(
      file = paste0("SimData/NoWT_YesCR_popArrayDF",G,"Gen",comp,"comp",mu,"mu", j, "j.csv"),
      header = TRUE)
    popArrayDF<-popArrayDF[,2:(ncol(popArrayDF)-1)] #Remove some unnecessary columns
    
    #No, I don't think that makes sense. 
    #Rather, make deterministic model for the comp mutations
    rate_succ_comp = min(1,  2 * s_comp * N * mu *n)
    
    NresList = rep(N, floor(1/rate_succ_comp)); #Waiting time until successful mutation comes along
    Nres = N 
    for (i in 1:1000){
      Nres = max(1,Nres * (1 - mu*n) - (N-Nres)*s_comp)
      NresList = c(NresList, Nres)
    }
    
    plot(NresList[1:1000], ylim=c(9900,N), xlim=c(0,200), type="l", col=1, main = paste0("mu ", mu, " j ", j))
    points(popArrayDF$Res, type = "l", col=2)
  }
}
#OK, so now the big question is whether this rough deterministic-ish approximation can 
#tell us the probability that WT comes up in the pop. 

#First, use the approximation to calculate the probability of escape. 
for (mu in muvalues){
  #mu = muvalues[2]
  print(paste0("mu ", mu, " NoWT_YesCR"))
  #Rather, make deterministic model for the comp mutations
  rate_succ_comp = min(1,  2 * s_comp * N * mu *n)
  
  NresList = rep(N, floor(1/rate_succ_comp)); #Waiting time until successful mutation comes along
  Nres = N 
  for (i in 1:1000){
    Nres = max(1,Nres * (1 - mu*n) - (N-Nres)*s_comp)
    NresList = c(NresList, Nres)
  }
  popfitness = (NresList*(1-c) + (N-NresList)*(1-(c*(1-comp))))/N
  ProbRaw = mu* NresList * 2* ((1/popfitness)-1)
  #Calculate real probability  (taking into account that it may have happened before)
  ProbReal = c()
  ProbNo = c()
  i=1
  ProbReal = c(ProbReal, ProbRaw[i]*1)
  ProbNo = c(ProbNo, 1-ProbReal)
  for (i in 2:length(ProbRaw)){
    ProbReal = c(ProbReal, ProbNo[i-1]*ProbRaw[i])
    ProbNo = c(ProbNo, ProbNo[i-1]-ProbReal[i])
  }
print(1-min(ProbNo))
}

#To check this, I need  to run many sims, (with WT and not CR?) to determine that probability in the sims. 
#Why is this interesting? To get an intuitive understanding of whether WT occurs  and 
# whether CR occurs when. 

