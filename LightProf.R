#Light profile plot splitter:

#This script reads in data from BB light cart and splits by plot
#Final product is a light profile for each plot, including border plots

#Many functions are created here that are used elsewhere
#As of 11/11, I'm considering creating another script that just establishes the functions, then calling them all here.
 
source('ImportCS.R') #TOA5 importer

lp.raw<-importCSdata('CR1000XSeries_1_Quantum.dat') #Raw data


#Main processing; read, split, average ####

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
combine<-function(dat, ts, split, doy=unique(ts$DOY)){

bigdat.all<-cbind(ts, dat)

bigdat<-bigdat.all[bigdat.all$DOY%in%doy,]


plot.combine<-merge(split, bigdat, by='dectime', all=TRUE)

                           
both.str<-max(min(which(!is.na(plot.combine$row))), min(which(!is.na(plot.combine$TIMESTAMP))))
both.end<-min(max(which(!is.na(plot.combine$row))), max(which(!is.na(plot.combine$TIMESTAMP))))

plot.cl<-plot.combine[both.str:both.end,]


return(plot.cl)

}
plot.cl<-combine(dat=lp.raw, ts=lp.ts, split=splits, doy=212)

#Split plots - most of the work is done by this function
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
      
    if(length(loc)==8){print(paste("row", uniquerows[o], "range", uniqueranges[a], "is doubled"))}
    else if(length(loc)==0){print(paste("row", uniquerows[o], "range", uniqueranges[a], "is missing"))}
    else{print(paste("row", uniquerows[o], "range", uniqueranges[a], "is irregular;","There are", length(which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])), "records with this label"))}
    
      #if there's only one 'end' or 'start' recording, grab the corresponding start/end
      if(length(which(dat$io[loc]=='e'))==2){ #if you only have end
        
        ends<-loc[which(dat$io[loc]=='e')] #loc at the end record to match
        tdiff<-abs(dat$dectime[loc]-mean(dat$dectime[loc[which(dat$io[loc]=='e')]]));tdiff[tdiff==0]<-NA #tdiff are the time differences; NaN out zeroes because those are the end times
        starts<-loc[which(tdiff==min(tdiff, na.rm=TRUE))] #which record is closest to the end times
        
        loc.rep<-c(starts, ends)
        
        
       print(paste("row", uniquerows[o], "range", uniqueranges[a], "is resolved by matching single end record"))
        
        
      }
      
      #if there's only one start and multiple ends (probably rare), same procedure
      if(length(which(dat$io[loc]=='s'))==2){ #if you only have end
        
        ends<-loc[which(dat$io[loc]=='s')] #loc at the ends
        tdiff<-abs(dat$dectime[loc]-mean(dat$dectime[loc[which(dat$io[loc]=='s')]]));tdiff[tdiff==0]<-NA #tdiff are the time differences; NaN out zeroes because those are the end times
        starts<-loc[which(tdiff==min(tdiff, na.rm=TRUE))] #which record is closest to the end times
        
        loc.rep<-c(starts, ends)
        
        print(paste("row", uniquerows[o], "range", uniqueranges[a], "is resolved by matching single start record"))
              
      }
      
      #if there are multiples of both, take latest matched pair that has a reasonable time recorded
      
      
      #Now do the thing you do for normal records with the correct timestamps
      ind<-c(min(loc.rep):max(loc.rep))
      datsub<-dat[ind,]
      
      bigdf<-rbind(bigdf, datsub)
      
      #flagging for noise
      ranges<-apply(datsub[,numcol],2,FUN=function(x) range(x)[2]-range(x)[1])[10:15]
      noiseflag<-length(which(ranges>noise.tol))
      
      meandf[count,]<-c(colMeans(datsub[,numcol], na.rm=TRUE), noiseflag)
      
      
      }
  }
}

if(out=='mean'){return(meandf)}else{return(bigdf)}

}
plotmeans<-plotsplit(plot.cl); #plotfull<-plotsplit(plot.cl, out='all')

#####

#Additional QC####
#North/south column
plotmeans$ns<-'n';
plotmeans.rowf<-(plotmeans$row-1)/4
plotmeans$ns[plotmeans.rowf %% 2 == 0]<-'s'

#quality filter
#will have options to filter on noise, length of record
#####

#Basic plot to check out a sample of profiles ####
plotdat<-plotmeans[,10:15]
par(mfrow=c(1,1), mar=c(4,4,1,1))
heights<-c(7,22,36,55,70,88)
colgr<-colorRampPalette(c('cyan', 'antiquewhite', 'orange'));colvec<-colgr(5)[as.numeric(cut(plotmeans$dectime, breaks=5))]
plot((as.numeric(plotdat[1,])*100)~heights, col='white', xlim=c(0,90), ylim=c(-5,120), xlab="Measurement height (in)", ylab="% total sunlight (unitless)")
for(i in sample(which(plotmeans$ns=='s'), 10)){
  lines((as.numeric(plotdat[i,]/max(plotdat[i,]))*100)~heights, col=colvec[i], lwd=2)

}
text(64,-4,"Light profile measurements (random sample of 10 plots)", cex=0.9, font=2)
legend(0,120, legend=c("morning", "afternoon"), col=c('cyan', 'orange'), lwd=2, cex=0.8, bty='n')

#For plotting raw data on a click
#plot(as.numeric(plotdat[sample(1:1050, 1),]), col=sample(1:8, 1), type='l')
#####


