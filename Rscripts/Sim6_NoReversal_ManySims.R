#This one to run many sims. Only needed outcome is yes/no WT is there. 
#This script is to have only comp mutations, and no reversal. It is meant to determine the prob of reversal happen later. 

library(dplyr)
library("ggplot2")
library(dplyr)
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

#for (comp in seq(0,1,0.2)){
#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=500; samplesize=10
cex_red=0.8
#muvalues<-1/N*c(0.005,0.05,0.5,5,50)
muvalues<-1/N*c(0.001,0.01,0.1,1,10)
NumSims=100
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
}

