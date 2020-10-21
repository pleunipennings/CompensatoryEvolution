#Trying to make a Muller plot 

N =10000 #total pop size
G = 500 #numgenerations
c = 0.1 #cost of resistance
comp = 0.5 #effect of compensatory mutation (0.5 means half of cost is compensated, 1.5 means that cost is more than compensated)
muvalues<-1/N*c(0.001,0.01,0.1,1,10)

redorange=c('#D7191C','#FDAE61')
blue = '#2C7BB6'
greens = c('#7fc97f', '#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
coulpopDF = c(redorange, rep(greens,10),blue)

for (mu in muvalues){
  for (j in 1:4){
    png(paste0("Output/MutatorEffectMutipleCompStrains_MullerLike",G,"Gen",comp,"comp", mu, "mu", j, "j",Sys.Date(), ".png"), width = 480, height =370)
    par(mar = c(2.5, 2.5, 4, 1.))
    par(mfrow=c(1,1))
    
    popArrayDF = read.csv(file = paste0("SimData/","popArrayDF",G,"Gen",comp,"comp",mu,"mu", j, "j.csv"))
    
    popArrayDF_Per=apply(popArrayDF[,2:(ncol(popArrayDF)-1)], 2, function(x){x/10000})
    #Sample10Array_Per=apply(Sample10Array[,2:ncol(Sample10Array)], 2, function(x){x})
    barplot(t(popArrayDF_Per[seq(1,500,20),]), col=coulpopDF , border=0, xlab="",
            #xaxt="n", 
            yaxt="n",
            main = paste0("mu*N = ", mu*N),cex.main=1.1)
    mtext(seq(0,500,100), at=c(0,5,10,15,20,25)*1.2,side = 1, line=.2)
    mtext("Generations",side=1,line=1.2)

dev.off()
  }}