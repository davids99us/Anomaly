#Doing the roc plot
pdf("figure-002.pdf")
library(pROC)
par(mfrow=c(2,2))
#The iteration plot
i=400
m.exp <- c(rep(0,i/4), rep(1,i/4),rep(0,i/4),rep(0.2,i/4))
values <- m.exp+rnorm(i)
o=sumseg(values,max.breaks=3,plot=TRUE)
#browser()



length=40000
breaks=data.table(length=rep(40,length/40))
breaks[,index:=cumsum(length)]
breaks[,size:=rnorm(.N)]
#breaks[,size:=rep(c(0.5,-0.5),.N/2)]
m.exp=c()
for (i in 1:nrow(breaks)) {
  m.exp=c(m.exp,rep(breaks[i]$size,breaks[i]$length))
}
data=m.exp+rnorm(length(m.exp))
index=1:length(m.exp)
#plot(index,data,type="l")
#lines(index,m.exp,col=2,lwd=4)
D=data.table(index=index,value=data)
cuts<-model.ss(D,max.breaks=10)
setkey(cuts,index)
setkey(breaks,index)
x=breaks[cuts]
x[is.na(length),length:=0]
x[length==40,length:=1]
AUC=auc(x$length,x$prob)
roc(x$length,x$prob,plot=T,main="ROC",auc.polygon=TRUE,print.thres=TRUE,print.thres.col=4)
# Print the AUC on the plot

#browser()


#Does the efficience tests

result=data.table()
iters=c(1000,5000,10000,50000,100000,300000,500000)
for (j in 1:2) {
for (i in iters) {
m.exp <- c(rep(0,i/4), rep(1,i/4),rep(0,i/4),rep(0.2,i/4))
m.data <- m.exp+rnorm(i)
core=system.time(o<-sumseg(m.data))
pelt=system.time(m.pelt<-cpt.mean(m.data, method = "PELT"))
bseg=system.time(m.binseg<-cpt.mean(m.data, method = "BinSeg"))
result=rbind(result,list(N=i,core=core[1],pelt=pelt[1],bseg=bseg[1]))
}}
result[,plot(N,pelt,log="",col=3,ylab="Time",main="Length")]
result[,points(N,core,col=2)]
result[,points(N,bseg,col=4)]
result[,mean(core),by=N][,lines(N,V1,col=2,lwd=4)] 
result[,mean(pelt),by=N][,lines(N,V1,col=3,lwd=4)] 
result[,mean(bseg),by=N][,lines(N,V1,col=4,lwd=4)] 
#browser()

result=data.table()
n=1000
for (j in 1:6) {
for (i in 1:9) {
  m.exp <- c(rep(0,1000), rep(1,1000),rep(0,1000),rep(0.2,1000))
  m.data <- m.exp+rnorm(i)
  core=system.time(o<-sumseg(m.data,max.breaks=i))
  #pelt=system.time(m.pelt<-cpt.mean(m.data, method = "PELT"))
  if (i<8) {
    if (i==7) Q=2001 else Q=3^i
    bseg=system.time(m.binseg<-cpt.mean(m.data, method = "BinSeg",Q=Q))
    result=rbind(result,list(B=3^i,core=core[3],bseg=bseg[3]))
  } else {
    result=rbind(result,list(B=3^i,core=core[3],bseg=NA))
  }
}}
result[,plot(B,core,log="xy",ylim=c(0.001,1),col=2,ylab="Time",main="Breaks")]
#result[,lines(N,pelt,col=3)]
result[,points(B,bseg,col=4)]
result[,mean(core),by=B][,lines(B,V1,col=2,lwd=4)] 
result[,mean(bseg),by=B][,lines(B,V1,col=4,lwd=4)] 


dev.off()
#browser()

