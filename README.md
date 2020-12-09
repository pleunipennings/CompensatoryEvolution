# CompensatoryEvolution
Sims to study compensatory evolution. Project with Ruth Herschberg and Brandon Ogbunu

***Plotting probability that WT wins. 

Here, CR (compensated reversal) is not allowed, I set fitness of CR to 0. 
Sims are done with Sim6_WT1_CR0_ManySims.R
Simulation function is in source("Rscripts/NewPopArray_RawFitnessArray.R")
Sim results witten to "SimData/ProbWTWin_Sims_NoCR.csv"
Calculations are done in DeterministicApproxProbWTWin.R
Results written in write.csv(x=ProbWTWinDet, file = "SimData/ProbWTWin_Det_NoCR.csv")
Results are made into figure in FigureProbWTWins.R
Written to jpeg(filename  = paste0("Output/ProbWTWins", Sys.Date(), ".jpeg"), units = "in", res = 100, width = 8, height = 10)


***Prob that WT occurs on res and on comp 

Here, CR (compensated reversal) IS allowed, I set fitness of CR to 1. 
Sim6_WT1_CR1_ManySims.R
Data written to write.csv(x=ProbWT_soft, file = "SimData/ProbWTSoft.csv")
Fig made with FigureProbWTSoft.R
Graph ProbWTSoft2020-11-12.pdf


***Example plots (muller like frequencies over time)
Made by MakingMullerLikePlots.R

***Main figure with sims and experimental data 
Made by Sim6_Mutators_MakeStackedBarplot.R
Which uses Rscripts/ExperimentalDataPlot.R
Also uses data from SimData which are written by Sim6_Mutators_UseRawFitnessNewPopArray


*** Note that in the code, we sometimes use p and sometimes comp for the compensatory effect.








