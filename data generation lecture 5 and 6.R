library("bbmle")
par(mar=c(5,5,1,1))
curve(-dbinom(x=5,size=10,prob=x,log=TRUE),from=0,to=1,
      xlab="Parameter value", 
      ylab="Negative log-likelihood",lwd=5,col="blue",cex.lab=2)

abline(v=0.5,col="red",lwd=5,lty=2)

cats<-read.csv("catfish_survival.csv")

par_test<-expand.grid(seq(from=0,to=0.4,by=0.01),seq(from=0.275,to=0.825,by=0.005))

ppo<-rep(NA,times=nrow(par_test))

for(i in 1:nrow(par_test)) {
ppo[i]=-sum(dbinom(x=cats$catfish_survival,size=1,
              prob=plogis(par_test[i,1]+par_test[i,2]*cats$water_depth),log=T))
}

intercept=par_test[,1]
slope=par_test[,2]
loglik=ppo

plot(intercept~slope,pch=19,cex=0.3)

plot3d(x=intercept,y=slope,z=loglik)
#

ReedfrogSizepred

Killed<-ReedfrogSizepred$Kill/10
TBL<-ReedfrogSizepred$TBL

Kill<-ReedfrogSizepred$Kill

plot(Killed~TBL)

tricker = function(x, a, b, t, min = 1e-04) {
  ifelse(x < t, min, b * ((x - t)/a * exp(1 - (x -
                                                   + t)/a)))
  }

NLL.tricker = function(a, b, t) {
   p.pred = tricker(TBL, a, b, t=8.7615)
   -sum(dbinom(Kill, size = 10, prob = p.pred, log = TRUE))
}




FSP.tricker = mle2(NLL.tricker, start = list(a = 0.4,
 b = 0.3, t = 8),"SANN")


NLL.tricker = function(a, b, t=8.7615) {
  p.pred = tricker(TBL, a, b, t=8.7615)
  -sum(dbinom(Kill, size = 10, prob = p.pred, log = TRUE))
}

a_seq=seq(from=2,to=3,by=0.05)
b_seq=seq(from=0,to=1,by=0.05)
griddy<-expand.grid(a_seq,b_seq)

a=griddy[,1]
b=griddy[,2]

tadlik<-rep(NA,times=nrow(griddy))

for(i in 1:nrow(griddy)) {
  tadlik[i]<-NLL.tricker(TBL,a=a[i],b=b[i])
  
}



plot3d(x=a[22:441],y=b[22:441],z=tadlik[22:441])
#
