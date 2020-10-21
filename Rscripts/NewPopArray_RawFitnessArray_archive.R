#This file just has the newPopArray function with rawfitnesarray in it. 

newPopArray <- function(N, mu, popArray, rawfitnessarray, n) { #n is number of comp mutations possible)
  PerfectCompindeces = which(rawfitnessarray==1) 
  PerfectCompindeces = PerfectCompindeces[PerfectCompindeces!=1 & PerfectCompindeces!=103] #do not include the WT and the regular RC line
  RegularCompindeces = which(rawfitnessarray!=1) #take all the indeces that have fitness lower than one
  RegularCompindeces = RegularCompindeces[RegularCompindeces!=2] #remove the resistant one
  compindeces=3:(length(popArray)-1) # index 3 until the end of the array minus 1 are the comp mutations. 
  
  fitnessArray <- popArray*rawfitnessarray #the fitness of the 4 types 
  popArray <- c(rmultinom(n = 1, size = N, prob = fitnessArray)) #replication
  #next mutation of 3 types
  numMutR_S = rbinom(n=1, size = popArray[2], prob = mu) #mutations from Res to Sensitive (reversal)
  numMutR_C = rbinom(n=1, size = popArray[2]-numMutR_S, prob = 1-(1-mu)^n) #should maybe be slightly diff #Mutations from Resistant to Compensated
  #PSP aug 2020 added -numMutR_S to make sure we don't get more mutants than individuals
  numMutC_SC = rbinom(n=1, size = sum(popArray[RegularCompindeces]), prob = mu) #MUtations from Comp to Comp +Reverted. 
  #only RegularCompindeces can mutate to comp reverted
  #PSP Aug 2020 ADD LINE TO make it possible for regular compensated lines to get a perfect comp mutation. 
  #CONTINUE HERE
  numMutC_PC = rbinom(n=1, size = sum(popArray[RegularCompindeces]) - numMutC_SC, prob = 1 - (1 -mu)^length(PerfectCompindeces)) #MUtations from Comp to Perfect Comp. excluding the ones that are already muated to revert
  print(paste0(c("numMutC_PC",numMutC_PC, sum(popArray[RegularCompindeces]), numMutC_SC)))
  
  #print(c(numMutR_S,numMutR_C,numMutC_SC))
  popArray[1]= popArray[1]+ numMutR_S #add revertants to 1
  popArray[2]= popArray[2]- numMutR_S - numMutR_C #take away mutants from 2
  popArray[length(popArray)]= popArray[length(popArray)]+ numMutC_SC #add reverted and comp to last position
  
  #where do the compensatory mutations fall? 
  compindicesMut<-c()
  #if (numMutR_C>length(compindeces)) numMutR_C=length(compindeces) #in case there are more mutations than spots (very high mut rate)
  compindicesMut<-sample(compindeces, size = numMutR_C, replace = TRUE) #Find random mut spots
  #print(compindicesMut)
  for (c in compindicesMut) popArray[c] = popArray[c]+1 #put mutations in random mut spot
  
  #Now I need to add the new perfect muts to PerfectCompindeces
  perfectCompindicesMut<-c()
  perfectCompindicesMut<-sample(RegularCompindeces, size = numMutC_PC, replace = TRUE) 
  print(perfectCompindicesMut)
  for (c in perfectCompindicesMut) popArray[c] = popArray[c]+1 #put mutations in random mut spot
  
  popArray<-c(popArray)
  #print("*******")
  #print(popArray)
  return(popArray)
}
