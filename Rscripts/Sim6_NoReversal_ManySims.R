#This one to run many sims. Only needed outcome is yes/no WT is there. 
#This script is to have only comp mutations, and no reversal. It is meant to determine the prob of reversal happen later. 

source("Rscripts/NewPopArray_RawFitnessArray.R")

#popArray<-c(10, N, rep(0, n), 0)
#types<-c("WT / Reverted", "RES" , "Comp" , "Comp+Reverted")

#Let's start with a set of parameters
if (TRUE){
  N =10000 #total pop size
  G = 100 #numgenerations
  c = 0.1 #cost of resistance
  comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
  mu = 0.03/N # mutation rate
  numSims = 100
  n=100 #n is number of possible different comp mutations
}#set parameters

ProbWTWinDF <- data.frame(mu = 0, N = 0, theta = 0, c = 0, comp = 0, n = 0, probWTWin = 0)

#for (comp in seq(0,1,0.2)){
#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=500;
muvalues<-1/N*c(0.001,0.01,0.1,1,10)
NumSims=1000
for (comp in c(0.1, 0.2, 0.5, 0.8)){
for (mu in muvalues){
  NumWTWin = 0
  for (j in 1:NumSims){#starting the sim and  plotting it! 
  #for (j in 1:30){#starting the sim and  plotting it! 
    popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
    compindeces=3:(length(popArray)-1)
    #rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
    rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 0) #Give WT 1 And CR 0 fitness so only comp and WT works. 
    WTList = c(); CompList = c()
    for (i in 1:G){
      popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
      WTList = c(WTList, popArray[1])
      CompList = c(CompList, sum(popArray[compindeces]))
    }
    if (popArray[1]>10) {NumWTWin = NumWTWin + 1}
    if (popArray[1]<11) {#print(j)
    #plot(WTList, type = "l", ylim = c(0,N), main = j)
    #points(1:G, CompList, type = "l", col = 2)
  }
  }
  print(paste0(mu*N, "  ", NumWTWin/NumSims))
  #listProbWTEscape = c(listProbWTEscape, NumWTWin/NumSims)
  ProbWTWinDF[nrow(ProbWTWinDF)+1, ]<-c(mu, N, mu*N, c, comp, n, NumWTWin/NumSims)
}
}

comp = 0.5
for (n in c(25, 50, 200)){
  for (mu in muvalues){
    NumWTWin = 0
    for (j in 1:NumSims){#starting the sim and  plotting it! 
      popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
      compindeces=3:(length(popArray)-1)
      #rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
      rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 0) #Give WT 1 And CR 0 fitness so only comp and WT works. 
      for (i in 1:G){
        popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
      }
      if (popArray[1]>10) NumWTWin = NumWTWin + 1
    }
    print(paste0(mu*N, "  ", NumWTWin/NumSims))
    #listProbWTEscape = c(listProbWTEscape, NumWTWin/NumSims)
    ProbWTWinDF[nrow(ProbWTWinDF)+1, ]<-c(mu, N, mu*N, c, comp, n, NumWTWin/NumSims)
  }
}
write.csv(x=ProbWTWinDF, file = "SimData/ProbWTWin_Sims_NoCR.csv")
