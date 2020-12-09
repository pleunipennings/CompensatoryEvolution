#Make figure for prob WT wins


ProbWTSoft = read.csv (file = "SimData/ProbWTSoft.csv", row.names = 1)
ProbWTSoft = ProbWT_soft
#View(ProbWTSoft)
#ProbWTSoft = ProbWTSoft[order(ProbWTSoft$theta),]
#View(ProbWTSoft)


for (G in c(100,500)){
  jpeg(file  = paste0("Output/ProbWTSoft_G_", G, "_", Sys.Date(), ".jpg"), width = 8, height = 10, units = "in", res = 300)
  par(mfrow=c(2,1))
  
  plot(ProbWTSoft$theta, ProbWTSoft$probWTSoft, log = "x", ylim = c(0,1), 
       type = "l", xlim=c(10^-4, 10), col = 0, 
       main =paste0("Prob. of WT reversal mutation on two backgrounds (n=100, G=", G, ")"), 
       xaxt="n", 
       yaxt = "n", xlab = "N * mu", ylab = "Prob. WT reversal on 2 backgrounds")
  axis(side = 1, at = c(10^-3, 10^-2, 10^-1, 1, 10), labels = c(10^-3, 10^-2, 10^-1, 1, 10), las = 1)
  axis(side = 2, at = seq(0,1,0.1), labels = seq(0,1,0.1), las = 1)
  
  n=100
  color = 0; xjiggle = 0.98; pch = 1
  for (comp in c(0.1, 0.2, 0.5, 0.8)){
    color = color + 1; xjiggle = xjiggle*1.02; pch = pch +1
    #  points(ProbWTWinDet$theta[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], 
    #        ProbWTWinDet$probWTWin[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], type = "l", col = color)
    subset = ProbWTSoft[ProbWTSoft$comp==comp & ProbWTSoft$n == n & ProbWTSoft$G == G, ]
    points(subset$theta*xjiggle, type = "b",
           subset$probWTSoft, pch = pch, cex = 1.5, col = color)
  }
  legend(x = 0.0001, 1, legend = paste("p =", c(0.1, 0.2, 0.5, 0.8)), col = 1:4, pch = 2:5, title = "effect of comp. mutations")
  
  plot(ProbWTSoft$theta, ProbWTSoft$probWTSoft, log = "x", ylim = c(0,1), 
       type = "l", xlim=c(10^-4, 10), col = 0, 
       main =paste0("Prob. of WT reversal mutation on two backgrounds (p=0.5, G=", G, ")"), 
       xaxt="n", yaxt = "n", xlab = "N * mu", ylab = "Prob WT wins")
  axis(side = 1, at = c(10^-3, 10^-2, 10^-1, 1, 10), labels = c(10^-3, 10^-2, 10^-1, 1, 10), las = 1)
  axis(side = 2, at = seq(0,1,0.1), labels = seq(0,1,0.1), las = 1)
  
  n=100; comp = 0.5
  color = 0; xjiggle = 0.98; pch = 1
  for (n in c(25,50,100,200)){
    color = color + 1; xjiggle = xjiggle*1.02; pch = pch +1
    #points(ProbWTWinDet$theta[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], 
    #       ProbWTWinDet$probWTWin[ProbWTWinDet$comp==comp & ProbWTWinDet$n == n], type = "l", col = color)
    subset = ProbWTSoft[ProbWTSoft$comp==comp & ProbWTSoft$n == n & ProbWTSoft$G == G, ]
    points(subset$theta*xjiggle, type = "b",
           subset$probWTSoft, pch = pch, cex = 1.5, col = color)
  }
  legend(x = 0.0001, 1, legend = paste("n =", c(25,50,100,200)), col = 1:4, pch = 2:5, title = "number of comp mutations")
  
  dev.off()
}
#abline (h = 0.01, lty = "dashed")


