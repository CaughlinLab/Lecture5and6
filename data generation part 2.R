library("bbmle")
par(mar=c(5,5,1,1))
curve(-dbinom(x=5,size=10,prob=x,log=TRUE),from=0,to=1,
      xlab="Parameter value", 
      ylab="Negative log-likelihood",lwd=5,col="blue",cex.lab=2)

abline(v=0.5,col="red",lwd=5,lty=2)



