source('ImportCS.R') #TOA5 importer



lp.raw<-importCSdata('CR1000XSeries_1_Quantum.dat')

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
lp.ts<-breaktime(lp.raw$TIMESTAMP)

#Read in splits

splitzip<-function(path){
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
splits<-splitzip('LP_07_31')

#Merge, clip

combine<-function(dat, ts, split){

bigdat<-cbind(ts, dat)
plot.combine<-merge(split, bigdat, by='dectime', all=TRUE)

both.str<-max(min(which(!is.na(plot.combine$row))), min(which(!is.na(plot.combine$TIMESTAMP))))
both.end<-min(max(which(!is.na(plot.combine$row))), max(which(!is.na(plot.combine$TIMESTAMP))))

plot.cl<-plot.combine[both.str:both.end,]


return(plot.cl)

}

plot.cl<-combine(dat=lp.raw, ts=lp.ts, split=splits)


plotsplit=function(dat, numcol=c(1:3,5:8,11:18), out='mean', noise.tol=200){
  
uniquerows<-unique(dat$row)[!is.na(unique(dat$row))]
uniqueranges<-unique(dat$range)[!is.na(unique(dat$range))]

bigdf<-data.frame()
meandf<-data.frame(matrix(nrow=length(uniquerows)*length(uniqueranges), ncol=length(numcol)+1)); colnames(meandf)<-c(colnames(dat[numcol]), 'noisy')

count<-0

for (o in 1:length(uniquerows)){
  for(a in 1:length(uniqueranges)){
    
    count<-count+1
    loc<-which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])
    
    if(length(loc)==4){
      
      ind<-c(min(loc):max(loc))
      datsub<-dat[ind,]
      
      bigdf<-rbind(bigdf, datsub)
      
      #flagging for noise
      ranges<-apply(datsub[,numcol],2,FUN=function(x) range(x)[2]-range(x)[1])[10:15]
      noiseflag<-length(which(ranges>noise.tol))
      
      meandf[count,]<-c(colMeans(datsub[,numcol], na.rm=TRUE), noiseflag)
      
      
        
    }
    else{
      
    if(length(loc)==8){print(paste("row", o, "range", a, "is doubled"))}
    else if(length(loc)==0){print(paste("row", o, "range", a, "is missing"))}
    else{print(paste("row", o, "range", a, "is irregular;","There are", length(which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])), "records with this label"))}
    
      }
  }
}

if(out=='mean'){return(meandf)}else{return(bigdf)}

}

plotmeans<-plotsplit(plot.cl); #plotfull<-plotsplit(plot.cl, out='all')

#North/south column
plotmeans$ns<-'n';
plotmeans.rowf<-(plotmeans$row-1)/4
plotmeans$ns[plotmeans.rowf %% 2 == 0]<-'s'


plotdat<-plotmeans[,10:15]

plot(as.numeric(plotdat[1,]), col='white', ylim=c(0,1.2))
for(i in 1:nrow(plotdat)){
  lines(as.numeric(plotdat[i,]/max(plotdat[i,])), col=sample(1:20, 1))

}

#next up (probably prior to plotting): 
#fixing duplicates
#Flaggging for noise

