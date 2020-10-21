

#This is code for the project with Ruti and Brandon. 
#I want to know what the probability of compensatory evolution is, given a certain set of parameters. 

#Let's start with a set of parameters

N =10000 #total pop size
f = 0.001 #fraction susceptible at beginning
G = 200 #numgenerations
c = 0.1 #cost of resistance
comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
mu = 1/N # mutation rate
numSims = 100

newPopArray <- function(N, c, comp, mu, popArray) {
  #first sampling
  fitnessArray <- popArray*c(1, 1-c, 1-(c*(1-comp)))
  popArray <- rmultinom(n = 1, size = N, prob = fitnessArray)
  #next mutation
  numMut = rbinom(n=1, size = popArray[2], prob = mu)
  popArray <- popArray + c(0, -numMut, +numMut)
}

newPopArrayVariableMutEffect <- function(N, c, comp, mu, popArray, mutrange) {
  #first sampling
  fitnessArray <- popArray*c(1, 1-c, 1-(c*(1-comp)))
  popArray <- rmultinom(n = 1, size = N, prob = fitnessArray)
  #next mutation
  numMut = rbinom(n=1, size = popArray[2], prob = mu)
  popArray <- popArray + c(0, -numMut, +numMut)
}

#starting the sim

#plot(1:G, 1:1000, type = "n", ylim=c(0,N))
#for (i in 1:G){
  #popArray<-newPopArray(N, c, comp, mu, popArray)
  #points(i, popArray[3], pch="*", col=2) #comp
  #points(i, popArray[1], pch="-", col=1) # susc
  #points(i, popArray[2], pch="+", col=3) #res
#}

compensationlist = (1:10)*0.1
fractionlist<-c(0,0.0001,0.001,0.01,0.1)
df<-data.frame(comp = 0, fraction = 0, numcomp = 0)

counter = 1
for (comp in compensationlist){
  for (fr in fractionlist){
    resultarray<-c()
    for (i in 1:numSims){
      S = rbinom(n = 1, size = N, prob = fr) #S is susceptible pop
      R = N-S #R is resistant pop
      Rc = 0 #starting with 0 compensated 
      popArray<-c(S, R, Rc) 
      for (j in 1:G){
        popArray<-newPopArray(N, c, comp, mu, popArray)
      }
      resultarray <- c(resultarray, popArray[3]>0.1*N)
    }
    df[counter,]<-c(comp, fr, length(which(resultarray)))
    counter = counter+1
  }
}

pdf("ProbCompEvolGivenSuscFractAndStrengthComp.pdf", width = 10)
plot(df$fraction+10^-5, df$numcomp/100, type="n", log="x", xaxt="n", xlab = "fraction susceptible",
     main = "Prob of comp. evolution, given fraction susceptible and strength of compensation")
axis(1, at=unique(df$fraction)+10^-5, labels=unique(df$fraction))
counter = 1
for (comp in compensationlist){
  print (comp)
points(df$fraction[df$comp==comp]+10^-5, df$numcomp[df$comp==comp]/100, type = "b", col=counter, lty = c(1:5,1:5,1:5)[counter])
counter = counter + 1
}
legend(x = 0.01, y = 1, legend = compensationlist, fill = 1:length(compensationlist), lty = 1:5)
dev.off()
