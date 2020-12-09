
#Let's start with a set of parameters
if (TRUE){
  N =10000 #total pop size
  G = 100 #numgenerations
  c = 0.15 #cost of resistance
  comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
  mu = 0.03/N # mutation rate
  numSims = 100
  n=100 #n is number of possible different comp mutations
}#set parameters

G=500
#First, use the approximation to calculate the probability of escape. 
manymuvalues = 10^(seq(-8,-3, by = 0.1))
ProbWTWinDet <- data.frame(mu = 0, N = 0, theta = 0, c = 0, comp = 0, n= 0, probWTWin = 0)
for (n in c(25, 50, 100, 200)){
  for (comp in c(0.1, 0.2, 0.5, 0.8)){
    for (mu in manymuvalues){
      print(paste0("mu ", mu, " N*mu ", N*mu, "  NoWT_YesCR"))
      #Calculate waiting time for successful comp mutation
      popfitness = 1-c #average population fitness
      s_comp = (1-(c*(1-comp)))/popfitness - 1 #selection coefficient comp mutation
      rate_succ_comp = min (1, 1 - (1-2*s_comp*mu)^(N*n)) #rate at which successful comp mutations happen
      waitingtime_succ_comp = min (G, floor(1/rate_succ_comp)) #average waiting time exponential
      print(waitingtime_succ_comp)
      waitingtime_succ_comp = 1 #if it is a deterministic approximation, maybe I should assume that the comp mut is there immediately?
      NresList = rep(N, waitingtime_succ_comp); #Nres doesn't change until first successful comp mut occurs. 
      Nres = N-1; Nc = 1;
      #for (i in 1:(G-waitingtime_succ_comp)){ #500 generations, just like the simulations # find Nres for G generations
      for (i in 1:500){ #500 generations, just like the simulations # find Nres for G generations
        popfitness = ( Nres*(1-c) + (N-Nres)*(1-(c*(1-comp))) )/N #average population fitness
        Nres = Nres * ((1-c)/ popfitness) * ((1-mu)^n)
        NresList = c(NresList, Nres)
      }
      popfitnessList = (NresList*(1-c) + (N-NresList)*(1-(c*(1-comp))))/N # population fitness 
      s_WT_List = 1/popfitnessList - 1 #selection coefficient WT mutation
      pfix = (1-exp(-2*s_WT_List))/(1-exp(-2*N*s_WT_List))
      ProbRawList = 1 - (1-2*s_WT_List*mu)^NresList #prob that a successful WT mutant arises
      #Calculate real probability  (taking into account that it may have happened before)
      i=1
      ProbReal = ProbRawList[i]*1
      ProbNo = 1-ProbReal
      for (i in 2:length(ProbRawList)){
        ProbReal = c(ProbReal, ProbNo[i-1]*ProbRawList[i])
        ProbNo = c(ProbNo, ProbNo[i-1]-ProbReal[i])
      }
      ProbWTWinDet[nrow(ProbWTWinDet)+1, ]<-c(mu, N, mu*N, c, comp, n, (1-min(ProbNo)))
    }
  }
}

ProbWTWinDet<-ProbWTWinDet[ProbWTWinDet$theta>0,]

write.csv(x=ProbWTWinDet, file = "SimData/ProbWTWin_Det_NoCR.csv")
