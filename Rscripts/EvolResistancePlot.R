#Trying to make a Muller plot FOR THE OPPOSITE DIRECTION, TO GET RESISTSNCE
#NEED TO INCLUDE MUTATION THE OTHER WAY. 

if (TRUE){
  N =10000 #total pop size
  G = 100 #numgenerations
  c = 0.05 #cost of resistance #make resistance beneficial for this sim
  comp = -1 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
  mu = 0.03/N # mutation rate
  numSims = 100
  n=100 #n is number of possible different comp mutations ZERO HERE
}#set parameters

#for (comp in seq(0,1,0.2)){
#Run one sim per mut rate and plot identity of 10 clones per sim. 
G=500; samplesize=10
cex_red=0.8
mu = 1/N*c(0.1)
Sample10Array<-data.frame(X=c("WT", "Res","CR", paste0("comp",1:10)))
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

redorange=c('#D7191C','#FDAE61')
orangered = c('#FDAE61','#D7191C')
blue = '#2C7BB6'
greens = c('#7fc97f', '#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
coulpopDFOPPOSITE = c(orangered, rep(greens,10),blue)

png(paste0("Output/EvolutionOfResistance",G,"Gen",comp,"comp",Sys.Date(), ".png"), width = 480, height =370)
par(mar = c(2.5, 2.5, 4, 1.))
par(mfrow=c(1,1))

popArrayDF=popArrayDF[,2:3]
popArrayDFMatrix=prop.table(data.matrix(popArrayDF),margin = 1)
#popArrayDF_Per=apply(popArrayDF, 2, function(x){x/sum(x)})
#Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x})
barplot(t(popArrayDFMatrix[seq(0,400,20),]), col=coulpopDFOPPOSITE , border=0, xlab="",
        #xaxt="n", 
        yaxt="n",
        main = "",cex.main=1.1)

dev.off()
