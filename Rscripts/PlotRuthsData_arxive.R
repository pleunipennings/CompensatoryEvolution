
#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is vs the prob of reversal  

ExpData<-read.csv("Individual_clone_outcome_data_summary.csv")
MutationRateValues <-read.csv("Individual_clone_outcome_data_mutationRates.csv")

pdf(paste0("ExperimentalData",Sys.Date(), ".pdf"), width = 10)

ExpData_Per=apply(ExpData[,2:ncol(ExpData)], 2, function(x){x*10/sum(x,na.rm=T)})

coul = c('#D7191C','#FDAE61','#2C7BB6','#7fc97f', '#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
xvalues=((1:5)-0.4)*1.2
barplot(ExpData_Per, col=coul , border="black", xlab="",xaxt="n", 
        main = paste0("Outcome in 5 experimental populations"))
mtext(side=3,at= mean(xvalues),paste0("Varying mutation rates"),cex=1.1,line=.3)
#mtext(side=1,at= xvalues,1:length(xvalues),cex=.85,line=.0)
mtext(side=1,at= xvalues[1:5],c("non-mutator", rep("mutator",4)),cex=1,line=.0)
mtext(side=1,at= xvalues[1:5],MutationRateValues[1,2:6],cex=1,line=1)
mtext(side=1,at=mean(xvalues), "Clone-specific mutation rates",cex=1.4,line=2.5)
  for (x in 1:length(ExpData_Per[1,])){
    
    if (ExpData_Per[1,x]) text(xvalues[x], (ExpData_Per[1,x]-0)/2  ,"Reverted", cex=1.15)
    if (ExpData_Per[2,x]) text(xvalues[x], (sum(ExpData_Per[1:2,x])+ExpData_Per[1,x])/2 ,"No change", cex=1.15)
    if (ExpData_Per[3,x]) text(xvalues[x], (sum(ExpData_Per[1:3,x])+sum(ExpData_Per[1:2,x]))/2    ,"CompRev", cex=1.15)
    for (k in 4:13){
      if (ExpData_Per[k,x]) text(xvalues[x], (sum(ExpData_Per[1:k,x])+sum(ExpData_Per[1:(k-1),x]))/2   ,"Comp", cex=1.15)}
  }
dev.off()

