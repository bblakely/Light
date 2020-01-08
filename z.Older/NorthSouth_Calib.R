source('ImportCS.R') #TOA5 importer

lp.calib.raw<-importCSdata('CR1000XSeries_1_Quantum.dat')

plotstuff<-TRUE
#Break down timestamp
breaktime<-function(timestamp){
  H<-as.numeric(format(timestamp, "%H"))
  M<-as.numeric(format(timestamp, "%M"))
  S<-as.numeric(format(timestamp, "%S"))
  DOY<-as.numeric(strftime(timestamp, "%j"))
  dectime<-H+(M/60)+(S/3600)
  ts<-data.frame(cbind(DOY,H, M, S, dectime))
  return(ts)
}
lp.calib.ts<-breaktime(lp.calib.raw$TIMESTAMP)


lp.calib.clip<-lp.calib.raw[lp.calib.ts$DOY==226,]; lp.calib.ts.clip<-lp.calib.ts[lp.calib.ts$DOY==226,]

#plot(lp.calib.clip$PPF2_Avg~lp.calib.ts.clip$dectime)

viewhr<-function(dat=lp.calib.clip, ts=lp.calib.ts.clip, hr, sens="PPF2_Avg"){
  
  index<-which((ts$H==hr&ts$M<20)|(ts$H==hr-1&ts$M>45))
  lp.h<-dat[index,]
  lp.h.ts<-ts[index,]
  senscol<-which(colnames(lp.h)==sens)
  
  #plot(lp.h[,senscol]~lp.h.ts$dectime, ylim=c(700,2200), main=paste(sens," " ,hr,":00", sep=''), ylab='', xlab='')
  
}


par(mfrow=c(3,3), mar=c(3,2,3,1))
sensvec<-colnames(lp.calib.clip)[6:10]
for(s in 1:6){
  sens<-sensvec[s]
for(i in 9:17){
viewhr(sens=sens, hr=i)
}
}


ctimes<-read.csv('Calib_times.csv');ctimes<-ctimes[ctimes$type=="open",]

combine<-function(dat, ts, split, doy=unique(ts$DOY)){
  
  bigdat.all<-cbind(ts, dat)
  
  bigdat<-bigdat.all[bigdat.all$DOY==doy & bigdat.all$BattV_Min!=0,]
  
  
  plot.combine<-merge(split, bigdat, by='dectime', all=TRUE)
  
  
  both.str<-max(min(which(!is.na(plot.combine$type))), min(which(!is.na(plot.combine$TIMESTAMP))))
  both.end<-min(max(which(!is.na(plot.combine$type)))+120, max(which(!is.na(plot.combine$TIMESTAMP))))
  
  plot.cl<-plot.combine[both.str:both.end,]
  
  
  return(plot.cl)
  
}

calib.cl<-combine(dat=lp.calib.clip, ts=lp.calib.ts.clip, split=ctimes, doy=226)

init<-which(!is.na(calib.cl$type))[diff(which(!is.na(calib.cl$type)))!=1]

numcol<-c(1,12:19)

meandf<-data.frame(matrix(nrow=length(init), ncol=length(numcol))); colnames(meandf)<-colnames(calib.cl[numcol])
bigdf<-data.frame()

count<-0
for(i in init){
  count<-count+1
  ind<-c((i+6):(i+36)) #20 seconds after each start time, 3 second lag
  datsub<-calib.cl[ind,]
  
  bigdf<-rbind(bigdf, datsub)
  meandf[count,]<-apply(datsub[,numcol],2,FUN='median')
    #c(colMeans(datsub[,numcol], na.rm=TRUE))
  
}


#experiment with plots to pull out stable data

if(plotstuff== TRUE){
par(mfrow=c(2,2))
limvec<-c(1100,1200,1600,1750,1850,1800,1550,1200)
loop<-0

for(i in unique(bigdf$H.y)){
  loop<-loop+1
  
plot(data=bigdf, PPF2_Avg~dectime, subset=which((bigdf$H.y==i& bigdf$M.y<30)|(bigdf$H.y==i-1& bigdf$M.y>45)),
     main=i, col='black', ylim=c(limvec[loop],(limvec[loop]+300)))
  points(data=bigdf, PPF3_Avg~dectime, col='blue',subset=which((bigdf$H.y==i& bigdf$M.y<30)|(bigdf$H.y==i-1& bigdf$M.y>45)))
  points(data=bigdf, PPF4_Avg~dectime, col='green',subset=which((bigdf$H.y==i& bigdf$M.y<30)|(bigdf$H.y==i-1& bigdf$M.y>45)))
  points(data=bigdf, PPF5_Avg~dectime, col='orange',subset=which((bigdf$H.y==i& bigdf$M.y<30)|(bigdf$H.y==i-1& bigdf$M.y>45)))
  points(data=bigdf, PPF_above_Avg~dectime, col='red',subset=which((bigdf$H.y==i& bigdf$M.y<30)|(bigdf$H.y==i-1& bigdf$M.y>45)))
  
  
}

}

datdf<-meandf[,5:9]

meandf.hr<-round(meandf$dectime)
datdf<-meandf[,5:9]
if(plotstuff=='TRUE'){
par(mfrow=c(4,2), mar=c(4,4,4,1))
for(i in which(ctimes$orientation=='S')){
  #choosing the max
  #plot(unlist(datdf[i,])/unlist(max(datdf[i,], na.rm=TRUE)), main=meandf.hr[i], type='l', ylim=c(0.90,1))
  #lines(unlist(datdf[i+1,])/unlist(max(datdf[i+1,], na.rm=TRUE)), col='red')
  #Forcing max light to be S6
  plot((unlist(datdf[i,])/unlist(datdf[i,5]))~c(2:6), main=paste(meandf.hr[i], ":00", sep=''), type='l', ylim=c(0.90,1.1), ylab="output relative to TOC sensor", xlab='sensor number (by increasing mounting height)')
  lines((unlist(datdf[i+1,])/unlist(datdf[i+1,5]))~c(2:6), col='red')
  legend(2,1.15, legend=c("Northward orientation", "Southward orientation"), col=c('red', 'black'), bty='n', cex=0.8, lwd=2, y.intersp = 0.1)
  #text(4,0.91, "Sensor output in full sun relative to top-of-canpy sensor")
  }
}

mults<-1/(datdf/datdf$PPF_above_Avg); ns<-rep(c("s", "n"), 8)
mult.df<-cbind(mults, meandf.hr, ns); colnames(mult.df)[6]<-'hr'

mults.s<-mult.df[mult.df$ns=='s',]; mults.n<-mult.df[mult.df$ns=='n',]

rm(list=setdiff(ls(), c('mult.df','mults.s', 'mults.n')))



