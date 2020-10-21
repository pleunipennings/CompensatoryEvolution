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
  n=10 #n is number of possible different comp mutations
}#set parameters

#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=100; samplesize=10
DF_Outcome<-data.frame("comp"=0, "Theta"=0,"MeanNumCompReverted"=0, "StDevNumCompReverted" = 0)

muvalues<-1/N*c(0.01, 0.1, 1, 10,100)
NumPossiblePerfectCompMuts = 0
m=0
for (comp in seq(0,1,0.1)){
  #for (comp in c(0,0.2,0.4)){
  rawfitnessarray=c(1, 1-c, rep(1-(c*(1-comp)),n), 1)
  print (paste0("comp: ",comp))
  for (mu in muvalues){
    print(paste0(c("mu",mu,mu*N)))
    
    Sample10Array<-data.frame(X=c("WT", "Res","CR"))
    
    numCR = c()
    for (j in 1:100){#starting the sim and  plotting it! 
      popArray<-c(0, N, rep(0, n), 0)#start with everyone resistant
      for (i in 1:G){
        popArray<-newPopArray(N, mu, popArray, rawfitnessarray, n)
        #print(popArray)  
      }
      
      sampleNumCR = rbinom(n = 1,size = 10,prob = popArray[length(popArray)]/N)
      numCR = c(numCR,  sampleNumCR) #take sample of 10 and determine num CR in it
    }
    print(numCR)
    MeanNumCR = mean(numCR)
    print(MeanNumCR)
    m=m+1
    DF_Outcome[m,]<-c(comp,mu*N,MeanNumCR, 0)
    #mean(Sample10Array_Per[3,]) is num comp reverted
  }}    
#dev.off()


if(TRUE){
  #  DF_OutcomeComplete = DF_Outcome
  #DF_Outcome = DF_Outcome [DF_Outcome$Theta<100,]
  #p = qplot(log(Theta* rep(c(0.85,0.9, 0.95, 1, 1.05,1.1, 1.15), each =5)) , MeanNumCompReverted, data = DF_Outcome, color = factor(NumPerfect),
  #          geom=c("line","point"), ylab="Number reverted in sample of 10", xlab = "10 log Theta")
  p = qplot(comp , MeanNumCompReverted, data = DF_Outcome, color = factor(Theta),
            geom=c("line","point"), ylab="Number compensated and reverted in sample of 10", xlab = "comp")
  
  p+ scale_colour_manual(values = c('#d73027','#f46d43','#fdae61','#fee090',
                                    #'#e0f3f8',
                                    '#abd9e9','#74add1','#4575b4'), 
                         name="# theta")+
    scale_y_discrete("Number compensated and reverted in sample of 10", 0:10, 0:10, 0:10)+
    #scale_x_discrete(name ="Dose (mg)",  limits=c("2","1","0.5"))
    #  scale_x_discrete(name="comp", log(unique(DF_Outcome$Theta)), unique(DF_Outcome$Theta), log(unique(DF_Outcome$Theta)))+
    #  geom_errorbar(aes(ymin=MeanNumCompReverted-StDevNumReverted, ymax=MeanNumCompReverted+StDevNumReverted), width=.2,
    #                position=position_dodge(.6)) 
    
    ggsave(
      paste0("Output/NumCompReverted_ErrorBars_G",G,"_", Sys.Date(),".pdf"),
      plot = last_plot(),
      width = 10)
}
