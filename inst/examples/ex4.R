#Doint the fig for Rutherglen
library(data.table)
library(reshape2)
library(segmented)
library(lubridate)

neighbours=c("74034","82001","82053","72097","82100","72023","82002","74110","74009",
             "74106","80043","81084","82016","74114","81049",
             "72150","74062","74039","74128","80015","72000","75028","75031","75032")

Target1=na.omit(fread("inst/extdata/IDCJAC0011_82039_1800_Data.csv",
                     skip=1,drop=c(1:2,7:8)))
Target2=na.omit(fread("inst/extdata/IDCJAC0010_82039_1800_Data.csv",
                     skip=1,drop=c(1:2,7:8)))

#Target1[,itime:=as.ITime("00:00")]
#Target2[,itime:=as.ITime("12:00")]
#Target=rbind(Target1,Target2)
Target1[,index:=as.IDate(paste(V3,V4,V5,sep='-'))][,value:=V6]
setkey(Target1,index)
Target2[,index:=as.IDate(paste(V3,V4,V5,sep='-'))][,value:=V6]
setkey(Target2,index)

Target=Target1[Target2]
Target[,value:=i.value-value]
Target[,plot(index,value,type="l")]
browser()
Target[,value:=V6]

Target[,year:=substr(index,1,4)]
ntarget=Target[,mean(value,na.rm=TRUE),by=year]
ACORN=data.table(na.omit(read.table("inst/extdata/acorn.sat.minT.82039.daily.txt",skip=1,na.strings="99999.9")))
ACORN[,index:=as.IDate(as.character(V1),format="%Y%m%d")][,year:=substr(index,1,4)]
Ayear=ACORN[,mean(V2),by=year]
setkey(Ayear,year)
setkey(ntarget,year)
Diff=Ayear[ntarget]
Diff[,adjust:=V1-i.V1]
#pdf("detail.pdf")
lines(ACORN$index,ACORN$V2,col=3)
setkey(Target,index)
setkey(ACORN,index)


#dev.off()
#browser()

 
#pdf("rutherglencore.pdf")
load("data/Rutherglen.Rda")
#Aggregate to year and plot
n<-Target[,plot(decimal_date(index),value,col="gray",type="l",ylab="temperature")]
ntarget=Target[,mean(value,na.rm=TRUE),by=year][,lines(year,V1,lwd=2)]
title("Rutherglen breaks - Core3")
#Predict with Core3 only
#browser()
cuts=model.core3(Target)
#Post process cuts by significance
levels=c(0.95,0.99,0.9999)
for (i in 2) {
  #browser()
  o2=predict.core3(cuts,Target,levels[i-1])
  browser()
  plot.core3(o2,cuts[prob>0.9999])
  #o2$data[,lines(decimal_date(index),fitted,col=i,lwd=1)]
  #browser()
}
#dev.off()
Diff[,lines(year,adjust+mean(ntarget$V1),col=5,lwd=3)]
      browser()


#Get the other stations for comparison
load("data/MComps.Rd")

MComp[,index:=as.IDate(index)]
MComp[,year:=substr(index,1,4)]
Prior=MComp[,.N,by=year]
#o=anomalyComp(Target,MComp[variable %in% neighbours,],breaks=6)
o=anomalyComp(Target,MComp,type="Core",sigma=4)
I=summarise.breaks(o$breaks,Prior)

pdf("rainbow.pdf")
Fig=MComp[variable %in% neighbours][,.(variable,mean(value)),by=.(year,variable)]
plot(c(1910,2015),c(0,0),xlim=c(1910,2015),ylim=c(5,12),ylab="Min Temperature",xlab="Years")
title("Rutherglen Comparators")
cols <- rainbow(length(neighbours))
for (i in 1:length(neighbours)) {
  P=Fig[variable==neighbours[i]]
  P[,lines(year,V2,col=cols[i])]
}
dev.off()
browser()
pdf("breaksAll.pdf")
ntarget[,plot(year,V1,ylim=c(4,10),col="gray")]
ntarget[,lines(year,V1,col="gray")]
I[,lines(year,freq+mean(ntarget$V1),lwd=1,col=3)]
I[,lines(year,value+mean(ntarget$V1),lwd=1,col=4)]
Diff[,lines(year,adjust+mean(ntarget$V1),col=5,lwd=3)]

title("Rutherglen Breaks")
dev.off()