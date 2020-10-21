

#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is, given a certain set of parameters. 

#Main function
newPopArrayVariableMutEffect <- function(N, c, mu, popArray, mutrange) {
  #first sampling
  fitnessArray <- popArray*c(1, 1-c, 1-(c*(1-comparray)))
  print(c(fitnessArray))
  popArray <- rmultinom(n = 1, size = N, prob = fitnessArray)
  print(c(popArray))
  #next mutation
  numMut = rbinom(n=1, size = popArray[2], prob = mu)
  print(numMut)
  typesofMut<-sample(x = 3:(length(popArray)), size = numMut, replace = TRUE)
  tabletypesofMut=c()
  for (i in 3:(length(popArray))){tabletypesofMut<-c(tabletypesofMut,length(which(typesofMut==i)))}
  print(tabletypesofMut)
  popArray[3:(length(popArray))]= popArray[3:(length(popArray))]+tabletypesofMut
  popArray[2] <- popArray[2] -numMut #remobve the mutants from the R column.
  popArray<-c(popArray)
  print(c(popArray))
  print("*******")
  return(popArray)
}


#Let's start with a set of parameters
if (TRUE){
N =10000 #total pop size
f = 0.001 #fraction susceptible at beginning
G = 100 #numgenerations
c = 0.1 #cost of resistance
comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
mu = 3/N # mutation rate
numSims = 100
comparray = seq(0.6,1.0,0.1)
}#set parameters

if (FALSE){
#starting the sim and  plotting it! 
G=200
S=10; R = N-S
popArray = c(S, R, rep(0,length(comparray)))
plot(1:G, 1:G, type = "n", ylim=c(0,N), main = "S = 10")
for (i in 1:G){
  popArray<-newPopArrayVariableMutEffect(N, c, mu, popArray, mutrange)
  for (j in 1:length(popArray)){
    points(i, popArray[j], pch="*", col=j) 
}}
}#run one sim and plot it

#Next step: think about what output I need. 
#Average fitness of thr R types at generation 50? 

#compensationlist = (1:10)*0.1
fractionlist<-c(0,0.0001,0.001,0.01,0.1)
df<-data.frame(fraction = 0, averagefitness = 0, fractionR_end=0)

counter = 1
#for (comp in compensationlist){
  for (fr in fractionlist){
    resultarray<-c()
    for (i in 1:numSims){
      S = rbinom(n = 1, size = N, prob = fr) #S is susceptible pop
      R = N-S #R is resistant pop
      popArray = c(S, R, rep(0,length(comparray)))
      for (j in 1:G){
        popArray<-newPopArrayVariableMutEffect(N, c, mu, popArray, mutrange)
      }
      RpopArray<-popArray[2:length(popArray)]
      averagefitnessR<-sum(RpopArray*c(1-c, 1-(c*(1-comparray))))/sum(RpopArray)
      resultarray <- c(resultarray, averagefitnessR)
      df[counter,]<-c(fr, averagefitnessR, sum(RpopArray)/N)
      counter = counter+1
      }
  }

pdf(paste0("AveFitnessRGivenSuscFractp_minComp=0.6_100gen_",Sys.Date(), ".pdf"), width = 10)
plot(df$fraction+10^-5, df$averagefitness, type="n", log="x", xaxt="n", xlab = "fraction susceptible",
     main = "Average fitness of Resistant bactaria, given fraction susceptible at start", ylim = c(1-c,1))
axis(1, at=unique(df$fraction)+10^-5, labels=unique(df$fraction))
for (i in 1:nrow(df)){
  points(df$fraction[i]+10^-5, df$averagefitness[i], pch=16, col="blue")
}
for (fr in fractionlist){
  aveaveFitnessR = mean(df$averagefitness[df$fraction==fr])
  points(fr+10^-5, aveaveFitnessR, pch=16, cex=3, col=1)
}
#legend(x = 0.01, y = 1, legend = compensationlist, fill = 1:length(compensationlist), lty = 1:5)
dev.off()


pdf(paste0("FractionR_endGivenSuscFractp_minComp=0.6_100gen_",Sys.Date(), ".pdf"), width = 10)
plot(df$fraction+10^-5, df$fractionR_end, type="n", log="x", xaxt="n", xlab = "fraction susceptible",
     main = "Average fraction of Resistant bactaria at gen 50, given fraction susceptible at start")
axis(1, at=unique(df$fraction)+10^-5, labels=unique(df$fraction))
for (i in 1:nrow(df)){
  points(df$fraction[i]+10^-5, df$fractionR_end[i], pch=16, col="blue")
}
for (fr in fractionlist){
  aveFractionR = mean(df$fractionR_end[df$fraction==fr])
  points(fr+10^-5, aveFractionR, pch=16, cex=3, col=1)
}
#legend(x = 0.01, y = 1, legend = compensationlist, fill = 1:length(compensationlist), lty = 1:5)
dev.off()
