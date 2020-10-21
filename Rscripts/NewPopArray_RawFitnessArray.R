#This file just has the newPopArray function with rawfitnesarray in it. 

newPopArray <- function(N, mu, popArray, rawfitnessarray, n) { #n is number of comp mutations possible)
  ####Reproduction first
  fitnessArray <- popArray*rawfitnessarray #the fitness of the 4 types 
  popArray <- c(rmultinom(n = 1, size = N, prob = fitnessArray)) #replication
  
  ####Get all  the indeces
  PerfectCompindeces = which(rawfitnessarray==1) 
  PerfectCompindeces = PerfectCompindeces[PerfectCompindeces!=1 & PerfectCompindeces!=103] #do not include the WT and the regular RC line
  RegularCompindeces = which(rawfitnessarray!=1) #take all the indeces that have fitness lower than one
  RegularCompindeces = RegularCompindeces[RegularCompindeces!=2] #remove the resistant one
  compindeces=3:(length(popArray)-1) # index 3 until the end of the array minus 1 are the comp mutations. 
  WTindex = 1
  RESindex = 2
  CompRevertindex = length(rawfitnessarray)
  NumPossiblePerfectCompMuts = length(PerfectCompindeces)
  
  ####Only RES and Comp can mutate. RES can mutate to WT, to comp and to perfect comp
  #Comp can mutate to perfect comp and CompRevert
  RESTotalMutProb = 1-(1-mu)^(n+1) # n+ 1 because n comp mutations and 1 WT mutation
  CompTotalMutProb = 1-(1-mu)^(NumPossiblePerfectCompMuts+1)
  
  ####Get number of mutants, then assign them. 
  numRESmuts = rbinom(n=1, size = popArray[RESindex], prob = RESTotalMutProb) #mutations from Res
  numCompmuts = rbinom(n=1, size = sum(popArray[RegularCompindeces]), prob = CompTotalMutProb) #mutations from Res
  
  #print(paste0(c("Res and comp muts",numRESmuts, numCompmuts)))
  
  ####Assign mutations using multinomial sampling
  RESmutsTargets = rep(0,length(rawfitnessarray))
  for (i  in c(WTindex, compindeces))  RESmutsTargets[i] = 1
  CompMutTargets = rep(0,length(rawfitnessarray))
  for (i in c(PerfectCompindeces, CompRevertindex)) CompMutTargets[i] = 1
  popArray = popArray + c(rmultinom(n = 1, size = numRESmuts, prob = RESmutsTargets)) + c(rmultinom(n = 1, size = numCompmuts, prob = CompMutTargets))
  
  #print(paste0(c("numCompmuts  ",numCompmuts, " numcomp ",sum(popArray[RegularCompindeces]))))
  ####Remove mutations, the RES mutations from RESindex and the Comp mutations from RegularCompIndeces
  if (numCompmuts>0){
    CompindicesOrigin = rep(0,length(rawfitnessarray))
    for (i in c(RegularCompindeces)) CompindicesOrigin[i] = 1
    for (i in 1:numCompmuts){
      popArray = popArray - c(rmultinom(n = 1, size = 1, prob = popArray*CompindicesOrigin)) #can only take what is there, that's why popArray*
    }
  }
  popArray[RESindex]=popArray[RESindex]-numRESmuts
  
  popArray<-c(popArray)
  #print("*******")
  #print(popArray)
  return(popArray)
}
