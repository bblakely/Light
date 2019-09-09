source('ImportCS.R') #TOA5 importer

lp.calib.raw<-importCSdata('CR1000XSeries_1_Quantum.dat')

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

plot(lp.calib.clip$PPF2_Avg~lp.calib.ts.clip$dectime)

viewhr<-function(dat=lp.calib.clip, ts=lp.calib.ts.clip, hr, sens="PPF2_Avg"){
  
  index<-which((ts$H==hr&ts$M<20)|(ts$H==hr-1&ts$M>45))
  lp.h<-dat[index,]
  lp.h.ts<-ts[index,]
  senscol<-which(colnames(lp.h)==sens)
  
  plot(lp.h[,senscol]~lp.h.ts$dectime, ylim=c(700,2200), main=paste(sens," " ,hr,":00", sep=''), ylab='', xlab='')
  
}


par(mfrow=c(3,3), mar=c(3,2,3,1))
sensvec<-colnames(lp.calib.clip)[6:10]
for(s in 1:6){
  sens<-sensvec[s]
for(i in 9:17){
viewhr(sens=sens, hr=i)
}
}


