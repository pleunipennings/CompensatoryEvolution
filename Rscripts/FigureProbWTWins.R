#Make figure for prob WT wins

ProbWTWinDet = read.csv(file = "SimData/ProbWTWin_Det_NoCR.csv", row.names = 1)
ProbWTWinDF = read.csv(file = "SimData/ProbWTWin_Sims_NoCR.csv", row.names = 1)

jpeg(filename  = paste0("Output/ProbWTWins", Sys.Date(), ".jpeg"), units = "in", res = 100, width = 8, height = 10)
par(mfrow=c(2,1))

plot(ProbWTWinDet$theta, ProbWTWinDet$probWTWin, log = "x", ylim = c(0,1), 
     type = "l", xlim=c(10^-4, 10), col = 0, 
     main =paste0("Prob WT wins, n=100"), 
     xaxt="n", yaxt = "n", xlab = "N * mu", ylab = "Prob WT wins")
axis(side = 1, at = c(10^-3, 10^-2, 10^-1, 1, 10), labels = c(10^-3, 10^-2, 10^-1, 1, 10), las = 1)
axis(side = 2, at = seq(0,1,0.1), labels = seq(0,1,0.1), las = 1)

n=100
color = 0; xjiggle = 0.98; pch = 1
for (comp in c(0.1, 0.2, 0.5, 0.8)){
  color = color + 1; xjiggle = xjiggle*1.02; pch = pch +1
  points(ProbWTWinDet$theta[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], 
         ProbWTWinDet$probWTWin[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], type = "l", col = color)
  points(ProbWTWinDF$theta[ProbWTWinDF$comp==comp & ProbWTWinDF$n == n]*xjiggle, 
         ProbWTWinDF$probWTWin[ProbWTWinDF$comp==comp & ProbWTWinDF$n == n], pch = pch, cex = 1.5, col = color)
}
legend(x = 0.5, 0.5, legend = paste("p =", c(0.1, 0.2, 0.5, 0.8)), col = 1:4, pch = 2:5)

plot(ProbWTWinDet$theta, ProbWTWinDet$probWTWin, log = "x", ylim = c(0,1), 
     type = "l", xlim=c(10^-4, 10), col = 0, 
     main =paste0("Prob WT wins, different n values"), 
     xaxt="n", yaxt = "n", xlab = "N * mu", ylab = "Prob WT wins")
axis(side = 1, at = c(10^-3, 10^-2, 10^-1, 1, 10), labels = c(10^-3, 10^-2, 10^-1, 1, 10), las = 1)
axis(side = 2, at = seq(0,1,0.1), labels = seq(0,1,0.1), las = 1)

n=100; comp = 0.5
color = 0; xjiggle = 0.98; pch = 1
for (n in c(25,50,100,200)){
  color = color + 1; xjiggle = xjiggle*1.02; pch = pch +1
  points(ProbWTWinDet$theta[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], 
         ProbWTWinDet$probWTWin[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], type = "l", col = color)
  points(ProbWTWinDF$theta[ProbWTWinDF$comp==comp & ProbWTWinDF$n == n]*xjiggle, 
         ProbWTWinDF$probWTWin[ProbWTWinDF$comp==comp & ProbWTWinDF$n == n], pch = pch, cex = 1.5, col = color)
}
legend(x = 0.5, 0.5, legend = paste("n =", c(25,50,100,200)), col = 1:4, pch = 2:5)

dev.off()

#abline (h = 0.01, lty = "dashed")


