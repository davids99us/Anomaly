#Doing   figure 1
#pdf("figure-001.pdf")
lty=2
col=3
l=4000
labels=c("A","B","C","D")
par(mfrow=c(2,2),mar=c(2,2,2,2))
m.exp <- c(rep(0,l/4), rep(1,l/4),rep(0,l/4),rep(0.2,l/4))
m.data <- m.exp+rnorm(l)


core=system.time(m.core<-sumseg(m.data))
n<-plot.ss(m.core$data)
lines(m.exp,col=col,lwd=4,lty=lty)
title("SumSeg")
m.core$cuts=na.omit(m.core$cuts)
m.core$cuts[,F1:=NULL][,i.F2:=NULL]
setorder(m.core$cuts,-prob)

segn=system.time(m.segn<-cpt.mean(m.data, method = "SegNeigh"))
plot(m.segn,cpt.width = 4)
title("SegNeigh")
lines(m.exp,col=col,lwd=4,lty=lty)

pelt=system.time(m.pelt<-cpt.mean(m.data, method = "PELT"))
plot(m.pelt,cpt.width = 4)
title("PELT")
lines(m.exp,col=col,lwd=4,lty=lty)

bseg=system.time(m.binseg<-cpt.mean(m.data, method = "BinSeg"))
plot(m.binseg, cpt.width = 4)
title("BinSeg")
lines(m.exp,col=col,lwd=4,lty=lty)
#dev.off()
print(rbind(core,segn,pelt,bseg)[,1])


