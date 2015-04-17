png("figure-003.png")
#Does various datasets

par(mfrow=c(2,2))
library(gstat)
data("wind",package="gstat")
m.p=cpt.mean(wind[,11],method="PELT",penalty="Asymptotic",pen.value=10^{-15})
plot(m.p,cpt.col="green",cpt.width=4,ylab="Velocity")
title("Wind Speed")
o=sumseg(wind[,11],max.breaks=3)
n<-o$data[,lines(index,fitted,col=2,lwd=4)]
#browser()

load(file="../data/PortHacking.Rda")
PH=na.omit(PH)
cuts=model.ss(PH,max.breaks=3)
PH=predict.ss(cuts,PH,sig=1)
#d=png("fig.png")
p<-plot.ss(PH)
title("Tide Height")
#d=dev.off()
#browser()

load("data/Rutherglen.Rda")
n<-Target[,plot(decimal_date(index),value,type="l",
                ylab="Temperature",xlab="Year")]
ntarget=Target[,mean(value,na.rm=TRUE),by=year][,lines(year,V1,lwd=4,col="white")]
title("Temperature")
cuts=model.ss(Target,max.breaks=3)
levels=c(0.9,0.95,0.99)
for (i in 2:4) {
  o2=predict.ss(cuts,Target,levels[i-1])
  o2[,lines(decimal_date(index),fitted,col=i,lwd=4)]
}
#browser()

TT=o2[index>as.IDate("1972-1-1") & index<as.IDate("1974-1-1")]
TT[,plot(decimal_date(index),value,type="l",main="Temperature - detail",xlab="Year")]
TT[,lines(decimal_date(index),fitted,col=4,lwd=4)]
dev.off()
