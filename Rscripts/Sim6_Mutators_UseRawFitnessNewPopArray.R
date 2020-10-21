
#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is vs the prob of reversal 
#June 2020: I am adding a chance that a comp mutation is perfect. 
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
comp = 0.1 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
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
#muvalues<-1/N*c(0.01,0.1)
for (mu in muvalues){
#for (mu in 1/N*c(0.1)){
    Sample10Array<-data.frame(X=c("WT", "Res","CR", paste0("comp",1:10)))
  for (j in 1:10){#starting the sim and  plotting it! 
    colnamesDF = c("Gen", "WT", "Res",paste0("comp", 1:n), "CR")
    popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
    popArrayDF <- as.data.frame(matrix(0, ncol = length(colnamesDF), nrow = 1))
    names(popArrayDF) = c("Gen", "WT", "Res",paste0("comp", 1:n), "CR")
    compindeces=3:(length(popArray)-1)
    rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
    for (i in 1:G){
      popArrayDF[i,]<-c(i,c(popArray))
      popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
    }
    #popArrayDF$TotalComp<-sum(popArrayDF[,])
    popArrayDF<-popArrayDF %>% mutate(sum = rowSums(.[compindeces+1]))
    write.csv(x = popArrayDF, file = paste0("SimData/","popArrayDF",G,"Gen",comp,"comp",mu,"mu", j, "j.csv"), row.names = FALSE)
    
  #run one sim and plot it
    #popArrayDF[150,2:(length(popArray)+1)]
    Sample10<-data.frame(t(rmultinom(n = 1, size = samplesize, prob = popArrayDF[nrow(popArrayDF),2:(length(popArray)+1)]))) #take sample of 10 clones
    numReverted = Sample10$WT
    numRes = Sample10$Res
    #numComp = sum(Sample10[grep(pattern = "comp", names(Sample10))])
    numCR = sum(Sample10$CR)
    #try to get info about softness of comp sweep
    CompSample<- Sample10[compindeces]
    cs<-CompSample[CompSample>0]
    Sample10Array[paste0("j",j)]<-c(numReverted, numRes, numCR, cs, rep(0,10-length(CompSample[CompSample>0])))
    #print(Sample10Array)
  }
  
  write.csv(x = Sample10Array, file = paste0("SimData/","Sample10Array",G,"Gen",comp,"comp",mu,"mu.csv"), row.names = FALSE)
}

