
#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is vs the prob of reversal 
#June 2020: I am adding a chance that a comp mutation is perfect. 
library(dplyr)

popArray<-c(10, N, rep(0, n), 0)
types<-c("WT / Reverted", "RES" , "Comp" , "Comp+Reverted")


#Main function
newPopArray <- function(N, c, mu, popArray, comp, n) { #n is number of comp mutations possible)
  fitnessArray <- popArray*c(1, 1-c, rep(1-(c*(1-comp)),n), 1) #the fitness of the 4 types 
  popArray <- c(rmultinom(n = 1, size = N, prob = fitnessArray)) #replication
  #next mutation of 3 types
  compindeces=3:(length(popArray)-1) 
  numMutR_S = rbinom(n=1, size = popArray[2], prob = mu) #mutations from Res to Sensitive (reversal)
  numMutR_C = rbinom(n=1, size = popArray[2], prob = mu*n) #should maybe be slightly diff #Mutations from Resistant to Compensated
  numMutC_SC = rbinom(n=1, size = sum(popArray[compindeces]), prob = mu) #MUtations from Comp to Comp +Reverted. 
  #print(c(numMutR_S,numMutR_C,numMutC_SC))
  popArray[1]= popArray[1]+ numMutR_S #add revertants to 1
  popArray[2]= popArray[2]- numMutR_S - numMutR_C #take away mutants from 2
  #where do the compensatory mutations fall? 
  compindicesMut<-c()
  #if (numMutR_C>length(compindeces)) numMutR_C=length(compindeces) #in case there are more mutations than spots (very high mut rate)
  compindicesMut<-sample(compindeces, size = numMutR_C, replace = TRUE) #Find random mut spots
  #print(compindicesMut)
  for (c in compindicesMut)popArray[c] = popArray[c]+1 #put mutations in random mut spot
  popArray[length(popArray)]= popArray[length(popArray)]+ numMutC_SC #add reverted and comp to last position
  popArray<-c(popArray)
  return(popArray)
}

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

#Run one sim per mut rate and plot freqs over time
if (FALSE){
#pdf(paste0("Output/MutatorEffectMutipleCompStrains",Sys.Date(), ".pdf"), width = 10)
pdf(paste0("Output/FreqsMutatorEffectMutipleCompStrains_10Clones",G,"Gen",Sys.Date(), ".pdf"), width = 7, height =9)
for (x in 1:3){
  for (mu in 1/N*c(0.01)){
#starting the sim and  plotting it! 
G=10000
colnamesDF = c("Gen", "WT", "Res",paste0("comp", 1:n), "CS")
popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
popArrayDF <- as.data.frame(matrix(0, ncol = length(colnamesDF), nrow = 1))
names(popArrayDF) = c("Gen", "WT", "Res",paste0("comp", 1:n), "CS")
compindeces=3:(length(popArray)-1)
for (i in 1:G){
  popArrayDF[i,]<-c(i,c(popArray))
  popArray<-newPopArray(N, c, mu, popArray, comp, n)
  }
#popArrayDF$TotalComp<-sum(popArrayDF[,])
popArrayDF<-popArrayDF %>% mutate(sum = rowSums(.[compindeces+1]))

#run one sim and plot it
plot(popArrayDF$Gen, popArrayDF$Res, type = "n", ylim=c(0,N*1.05), 
     main = paste0(c("cost= ",c,", comp= ", comp,", numcompmut= ", n, ", mu = ", mu), collapse = ""))
points(popArrayDF$Gen, popArrayDF$WT, type="l", col=1, lwd=2) #WT 
points(popArrayDF$Gen, popArrayDF$Res, type="l", col=2, lwd=2) #Res 
points(popArrayDF$Gen, popArrayDF$CS, type="l", col=3, lwd=2) #CS 
points(popArrayDF$Gen, popArrayDF$sum, type="l", col=4, lwd=2) #Compensated 
for (i in compindeces){
  popArrayDF<-popArrayDF %>% mutate(sum = rowSums(.[(compindeces+1)[1:(i-2)]]))
  points(popArrayDF$Gen, popArrayDF$sum, type="l", col=4, lty=2) #Compensated 
}
legend(x = 100, y = N*1.05,legend = c("Reverted", "Resistant", "Compensated and reverted", "Compensated"), fill=1:4)
  }
}
dev.off()
}

#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=100; samplesize=10
cex_red=0.8
pdf(paste0("Output/MutatorEffectMutipleCompStrains_10Clones",G,"Gen", "0.5Nmu",Sys.Date(), ".pdf"), width = 7, height =9)
par(mar = c(2.5, 2.5, 4, 1.))
par(mfrow=c(3,2))
muvalues<-1/N*c(0.005,0.05,0.5,5,50)
muvalues<-1/N*c(0.001,0.01,0.1,1,10)
for (mu in muvalues){
#for (mu in 1/N*c(0.1)){
    Sample10Array<-data.frame(X=c("WT", "Res","CR", paste0("comp",1:10)))
  for (j in 1:10){#starting the sim and  plotting it! 
    colnamesDF = c("Gen", "WT", "Res",paste0("comp", 1:n), "CR")
    popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
    popArrayDF <- as.data.frame(matrix(0, ncol = length(colnamesDF), nrow = 1))
    names(popArrayDF) = c("Gen", "WT", "Res",paste0("comp", 1:n), "CR")
    compindeces=3:(length(popArray)-1)
    for (i in 1:G){
      popArrayDF[i,]<-c(i,c(popArray))
      popArray<-newPopArray(N, c, mu, popArray, comp, n)
    }
    #popArrayDF$TotalComp<-sum(popArrayDF[,])
    popArrayDF<-popArrayDF %>% mutate(sum = rowSums(.[compindeces+1]))
    
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
  
  #library(RColorBrewer)
  #"#D7191C" "#FDAE61" "#ABD9E9" "#2C7BB6"
  coul = c('#D7191C','#FDAE61','#2C7BB6','#7fc97f', '#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
  xvalues=((1:10)-0.4)*1.2
  Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x*10/sum(x,na.rm=T)})
  #Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x})
  barplot(Sample10Array_Per, col=coul , border="black", xlab="",xaxt="n", yaxt="n",
          main = paste0("Outcome at 10 clones after ", G," generations"),cex.main=1.1)
  mtext(side=3,at= mean(xvalues),paste0("Pop-wide mutation rate mu*N = ", mu*N),cex=0.8,line=.3)
  mtext(side=1,at= xvalues,1:length(xvalues),cex=.75,line=.0)
  mtext(side=1,at=mean(xvalues) , "replicate population",cex=0.8,line=1.2)
  axis(2, at=0:10, labels=0:10, las=1)
  for (x in 1:length(Sample10Array_Per[1,])){
    
    if (Sample10Array_Per[1,x]) text(xvalues[x], (Sample10Array_Per[1,x]-0)/2  ,"WT", cex=0.8)
    if (Sample10Array_Per[2,x]) text(xvalues[x], (sum(Sample10Array_Per[1:2,x])+Sample10Array_Per[1,x])/2 ,"Res", cex=0.8)
    if (Sample10Array_Per[3,x]) text(xvalues[x], (sum(Sample10Array_Per[1:3,x])+sum(Sample10Array_Per[1:2,x]))/2    ,"CR", cex=0.8)
    for (k in 4:13){
    if (Sample10Array_Per[k,x]) text(xvalues[x], (sum(Sample10Array_Per[1:k,x])+sum(Sample10Array_Per[1:(k-1),x]))/2   ,"Com", cex=0.8)}
    }
}
dev.off()
