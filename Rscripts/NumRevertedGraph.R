library("ggplot2")
library(dplyr)
source("Rscripts/NewPopArray_RawFitnessArray.R")

#Let's start with a set of parameters
if (TRUE){
  N =10000 #total pop size
  #  G = 150 #numgenerations
  c = 0.1 #cost of resistance
  comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
  mu = 0.03/N # mutation rate
  numSims = 100
  n=100 #n is number of possible different comp mutations
}#set parameters

#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=100; samplesize=10
#pdf(paste0("Output/FractionReverted",G,"Gen",Sys.Date(), ".pdf"),height = 12, width = 12)
DF_Outcome<-data.frame("NumPerfect"=0, "Theta"=0,"MeanNumReverted"=0, "StDevNumReverted" = 0)
m=0
par(mfrow=c(1,1))
muvalues<-1/N*c(0.01, 0.1, 1, 10,100)
#muvalues<-1/N*c(100)
for (NumPossiblePerfectCompMuts in c(0:5, 10)){
#for (NumPossiblePerfectCompMuts in c(0,1)){
  rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
  PerfectCompindeces=c()
  if (NumPossiblePerfectCompMuts>0) PerfectCompindeces=3:(3+NumPossiblePerfectCompMuts-1) #make 0-10 comp mutations perfect
  rawfitnessarray[PerfectCompindeces]<-1
  #NumPossiblePerfectCompMuts = 10
  print (paste0("NumPossiblePerfectCompMuts: ",NumPossiblePerfectCompMuts))
  for (mu in muvalues){
  #for (mu in muvalues[length(muvalues)]){
    print(mu)
    Sample10Array<-data.frame(X=c("WT", "Res","CR", paste0("pcomp",1:10),paste0("comp",1:10)))
    for (j in 1:50){#starting the sim and  plotting it! 
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
    Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x*10/sum(x,na.rm=T)})
    #Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x})
    m=m+1
    DF_Outcome[m,]<-c(NumPossiblePerfectCompMuts,mu*N,mean(Sample10Array_Per[1,]+Sample10Array_Per[3,]), sd(Sample10Array_Per[1,]+Sample10Array_Per[3,]))
  }}    
#dev.off()

qplot(log(Theta), MeanNumReverted, data = DF_Outcome, color = factor(NumPerfect),
      geom=c("line","point"), ylab="Number reverted in sample of 10", xlab = "10 log Theta")+ 
  scale_colour_manual(values = c('#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4'), 
                      name="# perfectly compensating mutations")+
  scale_y_discrete("Number reverted in sample of 10", 0:10, 0:10, 0:10)+
  #scale_x_discrete(name ="Dose (mg)",  limits=c("2","1","0.5"))
  scale_x_discrete(name="N*mu", log(unique(DF_Outcome$Theta)), unique(DF_Outcome$Theta), log(unique(DF_Outcome$Theta)))
#geom_errorbar(aes(ymin=MeanNumReverted-StDevNumReverted, ymax=MeanNumReverted+StDevNumReverted), width=.2,
#         position=position_dodge(.6)) 

ggsave(
  paste0("Output/NumReverted_G",G,"_", Sys.Date(),".pdf"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE,
)

if(TRUE){
#  DF_OutcomeComplete = DF_Outcome
#DF_Outcome = DF_Outcome [DF_Outcome$Theta<100,]
p = qplot(log(Theta* rep(c(0.85,0.9, 0.95, 1, 1.05,1.1, 1.15), each =5)) , MeanNumReverted, data = DF_Outcome, color = factor(NumPerfect),
          geom=c("line","point"), ylab="Number reverted in sample of 10", xlab = "10 log Theta")

p+ scale_colour_manual(values = c('#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4'), 
                       name="# perfectly compensating mutations")+
  scale_y_discrete("Number reverted in sample of 10", 0:10, 0:10, 0:10)+
  #scale_x_discrete(name ="Dose (mg)",  limits=c("2","1","0.5"))
  scale_x_discrete(name="N*mu", log(unique(DF_Outcome$Theta)), unique(DF_Outcome$Theta), log(unique(DF_Outcome$Theta)))+
  geom_errorbar(aes(ymin=MeanNumReverted-StDevNumReverted, ymax=MeanNumReverted+StDevNumReverted), width=.2,
                position=position_dodge(.6)) 

ggsave(
  paste0("Output/NumReverted_ErrorBars_G",G,"_", Sys.Date(),".pdf"),
  plot = last_plot(),
  width = 10)
}

#Analytical prediction

if (FALSE){
DF_Outcome$PredictedNumReverted <-0

PredictedNumReverted<-function(theta_r, theta_c, s_r, s_c){
  return((theta_r*s_r)/(theta_r*s_r+s_c+theta_c*s_c))
}

for (i in 1:nrow(DF_Outcome)){
  theta_r = DF_Outcome$Theta[i]
  theta_c = DF_Outcome$Theta[i]*DF_Outcome$NumPerfect[i]
  DF_Outcome$PredictedNumReverted[i]<-10*PredictedNumReverted(theta_r, theta_c, 1/(1-c)-1, 1/(1-c)-1)
}
}
