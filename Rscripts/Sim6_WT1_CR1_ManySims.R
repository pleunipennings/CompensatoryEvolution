#This one to run many sims. Only needed outcome is yes/no WT is there. 
#This script is to have only comp mutations, and no reversal. It is meant to determine the prob of reversal happen later. 

source("Rscripts/NewPopArray_RawFitnessArray.R")
#types<-c("WT / Reverted", "RES" , "Comp" , "Comp+Reverted")

#Let's start with a set of parameters
if (TRUE){
  N =10000 #total pop size
  G = 100 #numgenerations
  c = 0.15 #cost of resistance
  comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
  mu = 0.03/N # mutation rate
  numSims = 100
  n=100 #n is number of possible different comp mutations
}#set parameters

ProbWT_soft <- data.frame(mu = 0, N = 0, theta = 0, c = 0, comp = 0, n = 0, probWTSoft = 0, G = 0)

#Run one sim per mut rate and plot identity of 10 clones per sim. 
for (G in c(100,500)){
  muvalues<-1/N*c(0.001,0.01,0.1,1,10)
  NumSims=1000; n = 100
  for (comp in c(0.1, 0.2, 0.5, 0.8)){
    for (mu in muvalues){
      #for (comp in c(0.2, 0.5)){
      #  for (mu in muvalues[3:4]){
      NumWTSoft = 0
      print (paste0(mu," - ", n," - ", comp))
      for (j in 1:NumSims){#starting the sim and  plotting it! 
        #for (j in 1:30){#starting the sim and  plotting it! 
        popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
        compindeces=3:(length(popArray)-1)
        #rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
        rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1) #Give WT 1 And CR 1 fitness so we can get WT on comp. 
        WTList = c(); CompList = c()
        for (i in 1:G){
          popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
        }
        #print(popArray)
        if (popArray[1]>10 & popArray[length(popArray)]>10) {NumWTSoft = NumWTSoft + 1}
      }
      print(paste0(mu*N, "  ", NumWTSoft/NumSims))
      #listProbWTEscape = c(listProbWTEscape, NumWTSoft/NumSims)
      ProbWT_soft[nrow(ProbWT_soft)+1, ]<-c(mu, N, mu*N, c, comp, n, NumWTSoft/NumSims, G)
    }
  }
  
  comp = 0.5
  for (n in c(25, 50, 200)){
    for (mu in muvalues){
      NumWTSoft = 0
      print (paste0(mu," - ", n," - ", comp))
      for (j in 1:NumSims){#starting the sim and  plotting it! 
        popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
        compindeces=3:(length(popArray)-1)
        rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1) #Give WT 1 And CR 0 fitness so only comp and WT works. 
        for (i in 1:G){
          popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
        }
        #print(popArray)
        if (popArray[1]>10 & popArray[length(popArray)]>10) {NumWTSoft = NumWTSoft + 1}
      }
      print(paste0(mu*N, "  ", NumWTSoft/NumSims))
      #listProbWTEscape = c(listProbWTEscape, NumWTSoft/NumSims)
      ProbWT_soft[nrow(ProbWT_soft)+1, ]<-c(mu, N, mu*N, c, comp, n, NumWTSoft/NumSims, G)
    }
  }
}

ProbWT_soft<-ProbWT_soft[!is.na(ProbWT_soft$theta),]
ProbWT_soft<-ProbWT_soft[ProbWT_soft$theta>0,]

write.csv(x=ProbWT_soft, file = "SimData/ProbWTSoft.csv")
