
#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is vs the prob of reversal 
#June 2020: I am adding a chance that a comp mutation is perfect. 
library(dplyr)

popArray<-c(1, 1, rep(1, n), 1)
rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)

types<-c("WT / Reverted", "RES" , "Comp" , "Comp+Reverted")
#Main function
source("Rscripts/NewPopArray_RawFitnessArray.R")

#Let's start with a set of parameters
if (TRUE){
  N =10000 #total pop size
#  G = 150 #numgenerations
  c = 0.1 #cost of resistance
  comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
  mu = 0.03/N # mutation rate
  numSims = 100
  n=20 #n is number of possible different comp mutations
}#set parameters

#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=100; samplesize=10
pdf(paste0("Output/MutatorEffectMutipleCompStrains_","Range", "Clones_WPerfectComp",G,"Gen",Sys.Date(), ".pdf"),height = 12, width = 12)
par(mfrow=c(3,4))
muvalues<-1/N*c(0.01, 0.1, 1, 10)
for (NumPossiblePerfectCompMuts in c(0, 1, 10)){
  rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
  PerfectCompindeces=c()
  if (NumPossiblePerfectCompMuts>0) PerfectCompindeces=3:(3+NumPossiblePerfectCompMuts-1) #make 0-10 comp mutations perfect
  rawfitnessarray[PerfectCompindeces]<-1
  #NumPossiblePerfectCompMuts = 10
  print (paste0("NumPossiblePerfectCompMuts: ",NumPossiblePerfectCompMuts))
  for (mu in muvalues){
    Sample10Array<-data.frame(X=c("WT", "Res","CR", paste0("pcomp",1:10),paste0("comp",1:10)))
    for (j in 1:10){#starting the sim and  plotting it! 
      colnamesDF = c("Gen", "WT", "Res",paste0("comp", 1:n), "CR")
      popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
      popArrayDF <- as.data.frame(matrix(0, ncol = length(colnamesDF), nrow = 1))
      names(popArrayDF) = colnamesDF
      compindeces=3:(length(popArray)-1)
      for (i in 1:G){
        popArrayDF[i,]<-c(i,c(popArray))
        popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
      }
      popArrayDF<-popArrayDF %>% mutate(sum = rowSums(.[compindeces+1]))
      Sample10<-data.frame(t(rmultinom(n = 1, size = samplesize, prob = popArrayDF[nrow(popArrayDF),2:(length(popArray)+1)]))) #take sample of 10 clones
      numReverted = Sample10$WT
      numRes = Sample10$Res
      #numComp = sum(Sample10[grep(pattern = "comp", names(Sample10))])
      numCR = sum(Sample10$CR)
      #try to get info about softness of comp sweep
      CompSample<- Sample10[compindeces[(NumPossiblePerfectCompMuts+1):length(compindeces)]]#nonperfect comp mutations
      cs<-CompSample[CompSample>0]
      CompSamplePerfect = c()
      if (NumPossiblePerfectCompMuts>0) CompSamplePerfect<- Sample10[compindeces[1: NumPossiblePerfectCompMuts]]#perfect comp mutations
      csp<-CompSamplePerfect[CompSamplePerfect>0]
      cs<-c(cs, rep(0,10-length(cs)))
      csp<-c(csp, rep(0,10-length(csp)))
      Sample10Array[paste0("j",j)]<-c(numReverted, numRes, numCR, csp, cs)
      #print(Sample10Array[,j+1])
    }
    
    #library(RColorBrewer)
    #"#D7191C" "#FDAE61" "#ABD9E9" "#2C7BB6"
    #Blue for CR '#2C7BB6'
    coul = c('#D7191C','#FDAE61',"hotpink", rep("grey",10), '#7fc97f', '#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
    xvalues=((1:10)-0.4)*1.2
    Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x*10/sum(x,na.rm=T)})
    #Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x})
    barplot(Sample10Array_Per, col=coul , border="black", xlab="",xaxt="n", 
            main = paste0("10 clones, ", G," generations"))
    mtext(side=3,at= mean(xvalues),paste0("mu*N = ", mu*N, "; num perfect: ", NumPossiblePerfectCompMuts),cex=1.1,line=.3)
    mtext(side=1,at= xvalues,1:length(xvalues),cex=.85,line=.0)
    mtext(side=1,at=mean(xvalues) , "replicate population",cex=1.3,line=1.5)
    #for (x in 1:length(Sample10Array_Per[1,])){
    #    if (Sample10Array_Per[1,x]) text(xvalues[x], (Sample10Array_Per[1,x]-0)/2  ,"WT", cex=1.15)
    #    if (Sample10Array_Per[2,x]) text(xvalues[x], (sum(Sample10Array_Per[1:2,x])+Sample10Array_Per[1,x])/2 ,"Res", cex=1.15)
    #    if (Sample10Array_Per[3,x]) text(xvalues[x], (sum(Sample10Array_Per[1:3,x])+sum(Sample10Array_Per[1:2,x]))/2    ,"CR", cex=1.15)
    #    for (k in 4:13){
    #    if (Sample10Array_Per[k,x]) text(xvalues[x], (sum(Sample10Array_Per[1:k,x])+sum(Sample10Array_Per[1:(k-1),x]))/2   ,"Comp", cex=1.15)}
    #    }
  }
}
dev.off()
