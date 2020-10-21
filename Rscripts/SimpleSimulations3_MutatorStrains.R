
#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is vs the prob of reversal  


#Main function
newPopArray <- function(N, c, mu, popArray, comp, n) { #n is number of comp mutations possible)
  fitnessArray <- popArray*c(1, 1-c, 1-(c*(1-comp)), 1) #the fitness of the 4 types
  #print(c(fitnessArray))
  popArray <- rmultinom(n = 1, size = N, prob = fitnessArray)
  #next mutation of 3 types
  numMutR_S = rbinom(n=1, size = popArray[2], prob = mu)
  numMutR_C = rbinom(n=1, size = popArray[2], prob = 1-(1-mu)^n) #should maybe be slightly diff
  numMutC_SC = rbinom(n=1, size = popArray[3], prob = mu) 
  #print(c(numMutR_S,numMutR_C,numMutC_SC))
  popArray[1]= popArray[1]+ numMutR_S
  popArray[2]= popArray[2]- numMutR_S - numMutR_C
  popArray[3]= popArray[3]+ numMutR_C - numMutC_SC
  popArray[4]= popArray[4]+ numMutC_SC 
  popArray<-c(popArray)
  print("*******")
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

pdf(paste0("MutatorEffect",Sys.Date(), ".pdf"), width = 10)

for (mu in 1/N*c(0.001,0.01,0.1,1,10,100)){
#starting the sim and  plotting it! 
G=200
popArray<-c(0, 10000,0,0) #start with everyone resistant
plot(1:G, 1:G, type = "n", ylim=c(0,N*1.05), 
     main = paste0(c("cost= ",c,", comp= ", comp,", numcompmut= ", n, ", mu = ", mu), collapse = ""))
for (i in 1:G){
  popArray<-newPopArray(N, c, mu, popArray, comp, n)
  for (j in 1:length(popArray)){
    points(i, popArray[j], pch="*", col=j) 
  }}#run one sim and plot it
legend(x = 100, y = N*1.05,legend = c("Reverted", "Resistant", "Compensated", "Compensated and reverted"), fill=1:4)
}

dev.off()

