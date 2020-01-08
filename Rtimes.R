#processing timestamps from R plot splitting

breaktime<-function(timestamp){
  H<-as.numeric(format(timestamp, "%H"))
  M<-as.numeric(format(timestamp, "%M"))
  S<-as.numeric(format(timestamp, "%S"))
  DOY<-as.numeric(strftime(timestamp, "%j"))
  dectime<-H+(M/60)+(S/3600)
  ts<-data.frame(cbind(DOY,H, M, S, dectime))
  return(ts)
}
  
splitzip<-function(path, report=FALSE){
  list<-list.files(path)
  splits<-data.frame()
  for(l in 1:length(list)){
    spl<-read.csv(paste(path,list[l], sep='/'), header=FALSE)
    times<-(as.POSIXlt(strptime(spl[,4],"%Y-%m-%d %H:%M:%S")))
    plots<-spl[,1:3]; colnames(plots)<-c('row', 'range','io')
      
    dectime<-breaktime(timestamp=times)$dectime
    splitnew<-cbind(dectime, plots)
    splits<- rbind(splits,splitnew);colnames
  }
  return(splits)
}

