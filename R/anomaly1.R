library(data.table)
library(changepoint)
library(lubridate)


#' sumseg
#' 
#' sumseg is fast breakpoints function 
#' allowing for bid dtasets This combines
#' predict.ss and summary.ss
#'
#' @param D timeseries or vector of data 
#' @param max.breaks The maximum number of breaks is 3^d
#' @param sig is the critical value for accepting
#' @param plot for plotting intermediate results
#' @return model fitted and the data.table of cuts  
#' @examples 
#' values=rnorm(400)+c(rep(-0.5,400/2),rep(0.5,400/2))
#' o=sumseg(values)
#' plot(1:400,values)
#' lines(o$data$index,o$data$fitted,col=2,lwd=4)
#' print(o$breaks) 

sumseg <- function(D, ...) UseMethod("sumseg")

sumseg.default <- function(D,max.breaks=4,sig=0.8163,plot=FALSE) {
  if (is.ts(D)) D=data.table(index=index(D),value=as.numeric(D))
  if (is.numeric(D)) D=data.table(index=1:length(D),value=D)
  #Gets the end ones as well - remove?
  #D=na.omit(D[,.(index,value)])
  #print(paste("No. Rows",nrow(D)))
  cuts=model.ss(D,max.breaks,plot)
  D=predict.ss(cuts,D,sig)
  out=summary.ss(cuts,D)
  setkey(cuts,prob)
  out$cuts=cuts
  class(out)="ss"
  out
}

#' model.ss
#' 
#' model.ss takes the data as a data.frame and returns cuts
#' should this be called something else?
#'
#' @param y timeseries or vector of data 
#' @param d The maximum number of breaks is 3^d
#' @param plot for plotting intermediate results
#' @return cuts the data.table of cuts  

model.ss<-function(D,max.breaks=4,plot=FALSE) {
  D=na.omit(D[,.(index,value)])
  #print(paste("No. Rows",nrow(D)))
D[,F1:=as.factor(D$index[1])]
cuts=data.table(value=NA,F1=factor(NA),F2=factor(NA),
                cusum=NA,crit=NA,index=D[,c(index[1],index[.N]+1)],
                i.F2=factor(NA),width=NA)
setkey(D,index)
if (plot==TRUE) {
  P=D[,index]
  M=data.table(max=NA,min=NA)
}
for (i in 1:max.breaks) {
  cuts[,F1:=F2]
  cuts=unique(cuts)
  D[,F2:=cut(index,cuts$index),by=F1]
  D[,cusum:=cumsum(scale(value)),by=F2]
  D[,crit:=crit(.N,1),by=F2]
  #Deletes rows less than 1SD
  D1=D[abs(cusum)>crit]
  #Gets peak max and width
  max=D1[,.(index=index[which.max(cusum)],width=.N),by=F2]
  min=D1[,.(index=index[which.min(cusum)],width=.N),by=F2]
  #Bug in data.tble should have equality
  setkey(max,index)
  setkey(min,index)
  #browser()
  cuts=rbind(cuts,D[max],D[min])
  setkey(cuts,index,crit)
  cuts=cuts[,.SD[1],by=index]
  if (plot==TRUE) {
    P=cbind(P,D[,cusum])
    M=rbind(M,list(max=max$index,min=min$index))
  }
}
#browser()
#cuts=na.omit(cuts)
cuts[,prob:=pnorm(abs(cusum),0,crit)]
cuts[,peak:=(abs(cusum)-crit)/width]
if (plot) {
  plot(P[,1],P[,2],type="l",col=1,xlab="Index",ylab="Cusum",lwd=3,main="Iterations")
  lines(P[,1],P[,3],col=2,lwd=3)
  lines(P[,1],P[,4],col=3,lwd=3)
  #cex=((cuts$prob-0.8)*10)
  M[2,lines(c(max,max),c(-1000,1000),lty=2)]
  M[2,lines(c(min,min),c(-1000,1000),lty=2)]
  #M[3,lines(c(max,max),c(-1000,1000),lty=2,col=2)]
  #M[3,lines(c(min,min),c(-1000,1000),lty=2,col=2)]
  #browser()
  #points(cuts$index,rep(0,nrow(cuts)),col=4,cex=1,pch=19)
}


cuts
}

#' predict.ss
#' 
#' predict.ss is fast breakpoints function 
#' allowing for bid dtasets This combines
#' predict.ss and summary.ss
#'
#' @param cuts the cutfile
#' @param sig is the critical value for accepting
#' @param data
#' @return fitted values
#' @return breaks table
#' @return cuts cut table

predict.ss<-function(cuts,D,sig=0.9,peak.ratio=0) {
  #cuts=cuts[prob>sig & !is.na(prob)]
  #browser()
  cuts=cuts[prob>=sig]
  cuts=cuts[peak>=peak.ratio]
  if (nrow(cuts)==0) {
    print(paste("No cuts >",sig))
    return(NULL)
  }
  new.cuts=c(D$index[1]-1,cuts$index,D$index[nrow(D)]+1)
  #browser()
  D[,F1:=cut(index,new.cuts)]
  D[,fitted:=mean(value,na.rm=TRUE),by=F1]
 D
}

#' summary.ss
#' 
#' summary.ss is fast breakpoints function 
#' allowing for bid dtasets This combines
#' predict.ss and summary.ss
#'
#' @param cuts the cutfile
#' @param data
#' @return data with fitted values

summary.ss<-function(cuts,D) {
  breaks=D[index %in% cuts$index]
  #breaks=D[index>=cuts$index & index<=cuts$index]
  level=diff(breaks$fitted)[1:(nrow(cuts)-2)]
  index=cuts[2:(.N-1)]$index
  return(list(breaks=data.table(index,level),data=D,cuts=cuts))
}


#Critical value of absolute cumulative of standard normal vector
#n is length and x is sigma

#crit<-function(n,x) x*(18.4+0.3483*sqrt(n))
crit<-function(n,x) x*sqrt(n)

#' plot.ss
#' 
#' plot breaks
#' 
#'
#' @param data data.table with fitted values

plot.ss<-function(data,cuts=NULL,n=100,ss.col=2,...) {
  if (is.null(cuts)) {  #Do the entire plot
    data[,plot(index,value,type="l",xlab="Year",...)]
    data[,lines(index,fitted,col=ss.col,lwd=3)]
  } else {  #Do panel of sub plots on breaks
    #cuts=cuts[prob>sig]
    setkey(data,index)
    setkey(cuts,index)
    temp=par()
    d=ceiling(sqrt(nrow(cuts)))
    par(mfcol=c(d,d),mar=c(1,1,1,1))
    data[,N:=.I]
  for (i in 1:nrow(cuts)) {
    row=data[cuts[i],N]
    D=data[N>row-n & N<row+n]
    if (nrow(D)>1) {
    D[,plot(index,value,type="l",xlab="Year",...)]
    D[,lines(index,fitted,col=ss.col,lwd=3)]
    #D[,lines(index,scale(cumsum(value)),col=5,lwd=1)]
    }
  #browser()
}
par=temp
}
}

#' anomalyComp
#' 
#' anomaly tests against a data.table of comparators
#' by fitting breaks to residuals and combining
#' outputs data.table of fitted breaks. 
#'
#' @param dates Vector of numeric time
#' @param values Target vector of values
#' @param tall table of comparators 
#' @param fitted Do you want fitted values returned
#' @param breaks The maximum number of breaks
#' @return breaks data.frame of breaks and location
#' @return data table of values, and fitted values

anomalyComp<-function(MTarget,MComp,breaks=4,type="Core",sigma=2) {
  setkey(MComp,date)
  setkey(MTarget,date)
  Diff=MComp[MTarget]
  Breaks=data.table()
  Fit=data.table()
  colors=rainbow(length(Comparators));j=1
  
  #Find the breaks for each residual
  for (i in MComp[,unique(variable)]) {
    C=Diff[variable==i,]
    print(paste("Doing",i,length(C$value)))
    C[,value:=lm(value~i.value)$residuals]
    #C=S[,.(date=date,value=scale(i.value-value,scale=FALSE))]
    o=switch(type,
             Seg=anomalySeg(C,breaks=breaks),
             Core=anomalyCore2(C,max.breaks=breaks,sigma=sigma),
             "Not a supported type")
    Breaks=rbind(Breaks,o$breaks)
    Fit=rbind(Fit,o$data[,variable:=i])
  }
  list(data=Fit,breaks=Breaks)
}


dolab<-function(lab="A") {
  p=par()
  text(p$xaxp[1],p$yaxp[2],lab)
}
