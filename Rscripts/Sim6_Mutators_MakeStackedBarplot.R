#This script uses data from SimData and makes a stacked barplot as used as Fig 1 in manuscript Sept 2020
N =10000 #total pop size
G = 100 #numgenerations
c = 0.1 #cost of resistance
comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
muvalues<-1/N*c(0.001,0.01,0.1,1,10)

#Note: in samplearray is Compensated & reverted at index 3. In popArrayDF juist op de laatste index. 

pdf(paste0("Output/MutatorEffectMutipleCompStrains_10Clones",G,"Gen",comp,"comp",Sys.Date(), ".pdf"), width = 7, height =9)
par(mar = c(3.5, 2.5, 4, 1.))
par(mfrow=c(3,2))
for (mu in muvalues){
  Sample10Array = read.csv(file = paste0("SimData/Sample10Array",G,"Gen",comp,"comp",mu,"mu.csv"),header = TRUE)
  #library(RColorBrewer)
  #"#D7191C" "#FDAE61" "#ABD9E9" "#2C7BB6"
  coul = c('#D7191C','#FDAE61','#2C7BB6','#7fc97f', '#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
  xvalues=((1:10)-0.4)*1.2
  Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x*10/sum(x,na.rm=T)})
  #Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x})
  barplot(Sample10Array_Per, col=coul , border="black", xlab="",xaxt="n", yaxt="n")
  title (paste0("Outcome in 10 simulated populations"),cex =1.1,line=2.3)
  mtext(side=3,at= mean(xvalues),paste0("Pop-wide mutation rate mu*N = ", mu*N),cex=0.8,line=.3)
  mtext(side=1,at= xvalues,1:length(xvalues),cex=.75,line=.0)
  mtext(side=1,at=mean(xvalues) , "replicate population",cex=0.8,line=1.2)
  #axis(2, at=0:10, labels=0:10, las=1)
  axis(2, at=0:10, labels=0:10/10, las=1)
  for (x in 1:length(Sample10Array_Per[1,])){
    if (Sample10Array_Per[1,x]) text(xvalues[x], (Sample10Array_Per[1,x]-0)/2  ,"WT", cex=0.8)
    if (Sample10Array_Per[2,x]) text(xvalues[x], (sum(Sample10Array_Per[1:2,x])+Sample10Array_Per[1,x])/2 ,"Res", cex=0.8)
    if (Sample10Array_Per[3,x]) text(xvalues[x], (sum(Sample10Array_Per[1:3,x])+sum(Sample10Array_Per[1:2,x]))/2    ,"CR", cex=0.8)
    for (k in 4:13){
      if (Sample10Array_Per[k,x]) text(xvalues[x], (sum(Sample10Array_Per[1:k,x])+sum(Sample10Array_Per[1:(k-1),x]))/2   ,"Com", cex=0.8)}
  }
}

#Add sixth plot from real data
source("Rscripts/ExperimentalDataPlot.R")

dev.off()
#}
