#Choose your sensors. Probably should functionalize this at some point.

sensors<-data.frame(cbind(c('downward', 'upward', 'downward', 'upward'), c('short', 'short', 'long','long')))

for(q in 1:4){

ud<-sensors[q,1]
lam<-sensors[q,2]

if(ud=="downward"&lam=="short"){filename<-'SR_08_09_2019/SR_08_09_data/SR_08_09_1153_relative.csv';nwave<-481}else
  if(ud=="upward"&lam=="short"){filename<-'SR_08_09_2019/SR_08_09_data/SR_08_09_1152_relative.csv';nwave<-481}else
    if(ud=="downward"& lam=="long"){filename<-'SR_08_09_2019/SR_08_09_data/SR_08_09_1025_relative.csv';nwave<-466}else
      if(ud=="upward"& lam=="long"){filename<-'SR_08_09_2019/SR_08_09_data/SR_08_09_1024_relative.csv';nwave<-466}else (print("Did not correctly specify sensor"))

print('##########################')
print(paste("Processing", sensors[q,1],sensors[q,2]))
print('##########################')

sr.raw<-read.csv(filename, skip=2, header=FALSE, nrows=nwave, colClasses = 'numeric')
sr.ts<-(read.csv(filename, header=FALSE, nrows=1))


###Deal with Apogee's dumb file format####
#de-factorize timestamp
i<-sapply(sr.ts,is.factor)
sr.ts[i]<-lapply(sr.ts[i], as.character)
sr.ts<-substr(sr.ts,1,19); sr.ts<-sr.ts[2:length(sr.ts)]
H<-as.numeric(substr(sr.ts,1,2)); M<-as.numeric(substr(sr.ts,4,5)); S<-as.numeric(substr(sr.ts,7,8))
dectime<-H+(M/60)+(S/3600)
TS<-data.frame(cbind(dectime,H, M, S))

#de-factorize and transpose data
sr.wave<-sr.raw[,1]
sr.num<-(t(sr.raw[2:ncol(sr.raw)]));colnames(sr.num)<-paste('b',sr.wave, sep="")

####


###Retrieve timestamps from plot splitting files####
source('Rtimes.R')
ts.split<-splitzip("SR_08_09_2019/SR_08_09_timestamp")
####

###Put everything together####
sr.dat<-as.matrix(cbind(TS, sr.num)); rownames(sr.dat)<-c();sr.pure<-sr.dat[,5:ncol(sr.dat)]
combine<-function(dat, ts, split, na.timecol="TIMESTAMP"){ #dat: stripped down data W/O timestamp info. ts: processed timestamp df. split: plot splitting df, formatted by breaktime+splitzip. na.timecol: column to use to indicate times of no data collection
  
  bigdat<-cbind(ts, dat)
  plot.combine<-merge(split, bigdat, by='dectime', all=TRUE)
  
  na.timeind<-which(colnames(plot.combine)==na.timecol)
  both.str<-max(min(which(!is.na(plot.combine$row))), min(which(!is.na(plot.combine[na.timeind]))))
  both.end<-min(max(which(!is.na(plot.combine$row))), max(which(!is.na(plot.combine[na.timeind]))))
  
  plot.cl<-plot.combine[both.str:both.end,]
  
  
  return(plot.cl)
  
}
plot.cl.sr<-combine(dat=sr.pure, ts=(TS), split=(ts.split), na.timecol="S")
#bit of cleanup
rm('sr.pure','sr.num','TS','ts.split', 'H','M','S', 'sr.ts')
####







###Actual plot splitting####
plotsplit=function(dat, numcol=c(1:3,5:8,11:18), out='mean', noise.tol=200, n.split=4){
  
  uniquerows<-unique(dat$row)[!is.na(unique(dat$row))]
  uniqueranges<-unique(dat$range)[!is.na(unique(dat$range))]
  
  #bigdf<-data.frame()
  meandf<-data.frame(matrix(nrow=length(uniquerows)*length(uniqueranges), ncol=length(numcol)+3)); colnames(meandf)<-c('row', 'range','count',colnames(dat[numcol]))#, 'noisy')
  
  count<-0
  
  for (o in 1:length(uniquerows)){
    for(a in 1:length(uniqueranges)){
      
      count<-count+1
      loc<-which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])
      
      if(length(loc)==n.split){ #If you have the right number of records
        
        ind<-c(min(loc):max(loc))
        datsub<-dat[ind,]
        
        datcount<-length(which(!is.na(rowMeans(datsub[,numcol]))))
        datlab<-c(uniquerows[o],uniqueranges[a], datcount);names(datlab)<-c('row', 'range', 'reccount')
        
        
        #if(out!='mean'){bigdf<-rbind(bigdf, datsub)}
        
        meandf[count,]<-c(datlab, colMeans(datsub[,numcol], na.rm=TRUE))#, noiseflag)
        
        
        
      }else 
        
      if (length(loc)<n.split){ #if you don't have enough records to collect the plot
        print(paste("incomplete data for row", uniquerows[o], "range",  uniqueranges[a]));print(("No resolution :("));print("*****")}
      
      else{
        
        print(paste("row",uniquerows[o], "range",  uniqueranges[a], "is irregular;","There are", length(which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])), "records with this label"))
        
        
        #if there's only one 'end' or 'start' recording, grab the corresponding start/end
        if(length(which(dat$io[loc]=='e'))==1){ #if you only have end
          
          ends<-loc[which(dat$io[loc]=='e')] #loc at the end record to match
          tdiff<-abs(dat$dectime[loc]-mean(dat$dectime[loc[which(dat$io[loc]=='e')]]));tdiff[tdiff==0]<-NA #tdiff are the time differences; NaN out zeroes because those are the end times
          starts<-loc[which(tdiff==min(tdiff, na.rm=TRUE))] #which record is closest to the end times
          
          if(!is.finite(min(tdiff, na.rm=TRUE))){print(paste("check row", uniquerows[o], "range", uniqueranges[a]))}
          
          loc.rep<-c(starts, ends)
          
          
          print(paste("row", uniquerows[o], "range", uniqueranges[a], "is resolved by matching single end record"))
          print('*****')
          
        }else
        
        #if there's only one start and multiple ends (probably rare), same procedure
        if(length(which(dat$io[loc]=='s'))==1){ #if you only have end
          
          ends<-loc[which(dat$io[loc]=='s')] 
          tdiff<-abs(dat$dectime[loc]-mean(dat$dectime[loc[which(dat$io[loc]=='s')]]));tdiff[tdiff==0]<-NA #tdiff are the time differences; NaN out zeroes because those are the end times
          starts<-loc[which(tdiff==min(tdiff, na.rm=TRUE))] #which record is closest 
          
          if(!is.finite(min(tdiff, na.rm=TRUE))){print(paste("check row", uniquerows[o], "range", uniqueranges[a]))}
          
          loc.rep<-c(starts, ends)
          
          print(paste("row", uniquerows[o], "range", uniqueranges[a], "is resolved by matching single start record"))
          print('*****')
        }else
        
          
        #if there are multiple starts and ends, pick the latest end and the matching start
        if(length(which(dat$io[loc]=='s'))>1 & length(which(dat$io[loc]=='e'))>1){
          
          
          ends<-loc[which(dat$dectime[loc]==max(dat$dectime[loc[which(dat$io[loc]=='e')]]))]
          tdiff<-(dat$dectime[loc[which(dat$io[loc]=='s')]])-dat$dectime[ends]
          starts<-loc[which(tdiff==max(tdiff[tdiff<0]))] #which record is closest, but still before 
        
          loc.rep<-c(starts, ends)
          
          print(paste("row", uniquerows[o], "range", uniqueranges[a], "is resolved by finding latest pair of records"))
          print('*****')
          
        }else{print ("No resolution :(");print("*****")}
        
      
        
        
        
        #Now do the thing you do for normal records with the corrected timestamps
        ind<-c(min(loc.rep):max(loc.rep))
        datsub<-dat[ind,]
        
        datcount<-length(which(!is.na(rowMeans(datsub[,numcol]))))
        datlab<-c(uniquerows[o],uniqueranges[a], datcount);names(datlab)<-c('row', 'range', 'reccount')
        
        
        #bigdf<-rbind(bigdf, datsub)
        meandf[count,]<-c(datlab, colMeans(datsub[,numcol], na.rm=TRUE))
        
        
      }
    }
  }
  
  if(out=='mean'){return(meandf)}else{return(bigdf)}
  
}
plotmeans.sr<-plotsplit(dat=plot.cl.sr, numcol=c(1,8:(nwave+7)), n.split=2) #plotfull<-plotsplit(plot.cl, out='all')


####



###Plot result####
# par(mfrow=c(1,1))
# band<-c(5:(nwave+4))
# plot(unlist(plotmeans.sr[500,band])~sr.wave, ylim=c(-100,5000), type='l', col='white')
# for(i in sample(1:(nrow(plotmeans.sr)), 200)){
#   lines(unlist(plotmeans.sr[i,band])~sr.wave, col=sample(1:8, 1))
# }

if(ud=="upward"&lam=="short"){plotmeans.sr->ul.sw.mean;sr.wave.sw<-sr.wave} ; if(ud=="downward"&lam=="short"){plotmeans.sr->dl.sw.mean;sr.wave.sw<-sr.wave}
if(ud=="upward"&lam=="long"){plotmeans.sr->ul.lw.mean;sr.wave.lw<-sr.wave} ; if(ud=="downward"&lam=="long"){plotmeans.sr->dl.lw.mean;sr.wave.lw<-sr.wave}

####


#Reflectance calculations for shotwave####
if(exists('ul.sw.mean')&exists('dl.sw.mean')&lam=="short"){

dl.sw.dat<-dl.sw.mean[,5:(nwave+4)]; ul.sw.dat<-ul.sw.mean[,5:(nwave+4)]
refl.sw<-dl.sw.dat/ul.sw.dat

colgr<-colorRampPalette(c('cyan','antiquewhite', 'orange')); colvec<-colgr(30)[as.numeric(cut(dl.sw.mean$dectime, breaks=30))]
#par(mfrow=c(1,1))
plot(unlist(refl.sw[1,])~sr.wave.sw, col='white', ylim=c(-0.4, 0.9))
for(i in sort(sample(1:nrow(refl.sw), 50))){
  exists<-!is.na(mean(unlist(refl.sw[i,]),na.rm=TRUE))
  if(exists==TRUE){lines(unlist(refl.sw[i,])~sr.wave.sw, col=colvec[i])}else(print(paste("row ", dl.sw.mean$row[i],',', "range ",dl.sw.mean$range[i], " does not have data", sep='')))
}

}

#Reflectance calculations for longwave####
if(exists('ul.lw.mean')&exists('dl.lw.mean')&lam=="long"){
  
  dl.lw.dat<-dl.lw.mean[,5:(nwave+4)]; ul.lw.dat<-ul.lw.mean[,5:(nwave+4)]
  refl.lw<-dl.lw.dat/ul.lw.dat
  
  colgr<-colorRampPalette(c('cyan','antiquewhite', 'orange')); colvec<-colgr(30)[as.numeric(cut(dl.lw.mean$dectime, breaks=30))]
  #par(mfrow=c(1,1))
  plot(unlist(refl.lw[1,])~sr.wave.lw, col='white', ylim=c(-0.4, 0.9))
  for(i in sort(sample(1:nrow(refl.lw), 50))){
    exists<-!is.na(mean(unlist(refl.lw[i,]),na.rm=TRUE))
    if(exists==TRUE){lines(unlist(refl.lw[i,])~sr.wave.lw, col=colvec[i])}else(print(paste("row ", dl.lw.mean$row[i],',', "range ",dl.lw.mean$range[i], " does not have data", sep='')))
  }
  
}
}


ref.ts<-dl.lw.mean[,1:4] #Grab a timestamp we know is in the right order for reflectance


#CLEANUP
rm('dl.lw.dat','ul.lw.dat','dl.sw.dat','ul.sw.dat','plot.cl.sr', 'sr.dat','sr.raw','dl.lw.mean','ul.lw.mean','dl.sw.mean','ul.sw.mean','sensors')


###Combine the spectra#####

ovlp.sw.ind<-which(colnames(refl.sw)%in%colnames(refl.lw));ovlp.lw.ind<-which(colnames(refl.lw)%in%colnames(refl.sw))

sw.onl<-refl.sw[,(1:(min(ovlp.sw.ind[1])-1))] #wavelengths only in the shortwave

#merge by weighted means
ovlp.sw<-refl.sw[ovlp.sw.ind];ovlp.lw<-refl.lw[ovlp.lw.ind]

ovlp<-matrix(data=0,nrow=nrow(ovlp.sw), ncol=ncol(ovlp.sw))

weightvec<-seq(from=0, to=1, length.out=length(ovlp.lw.ind))
for(l in 1:length(ovlp.sw.ind)){
  ovlp[,l]<-(ovlp.sw[,l]*(1-weightvec[l]))+(ovlp.lw[,l]*weightvec[l])
}

ovlp<-data.frame(ovlp);colnames(ovlp)<-colnames(ovlp.lw)

lw.onl<-refl.lw[,(max(ovlp.lw.ind)+1):ncol(refl.lw)]

refl.merge<-cbind(sw.onl,ovlp,lw.onl);sr.wave<-unique(c(sr.wave.sw,sr.wave.lw))

#MORE cleanup
rm('ovlp','ovlp.lw','ovlp.sw','lw.onl','sw.onl', 'plotmeans.sr')

#####

#Plot resulting complete spectra####
# plot(unlist(refl.merge[1,])~sr.wave, col='white', ylim=c(-0.1, 0.9), xlim=c(420,1010), ylab="Reflectance", xlab="Wavelength"); 
# text(500,0.85,"Reflectance Spectra
# (random sample of 10 plots)", font=2, cex=0.8)

for(i in sort(sample(1:nrow(refl.merge), 10))){
  exists<-!is.na(mean(unlist(refl.merge[i,]),na.rm=TRUE))
  if(exists==TRUE){
    lines(unlist(refl.sw[i,])~sr.wave.sw, col='gray', lwd=0.5, lty=3);
    lines(unlist(refl.lw[i,])~sr.wave.lw, col='black', lwd=0.5, lty=3)
    lines(unlist(refl.merge[i,])~sr.wave, col=colvec[i], lwd=2)
    }else(print(paste("row ", dl.lw.mean$row[i],',', "range ",dl.lw.mean$range[i], " does not have data", sep='')))
}
abline(v=c(max(sr.wave.sw), min(sr.wave.lw)));
text(c(560,725,890),-0.1, c("SW only  <<", ">>    overlap    <<", ">>  LW only"), cex=0.8)
legend(400,0.6,legend=c("morning","afternoon","unmerged shortwave", "unmerged longwave"),
       col=c('cyan','orange','gray', 'black'),lwd=c(2,2,0.5,0.5), lty=c(1,1,3,3), bty='n', cex=0.8)
#####


refl<-cbind(ref.ts[,1:4], refl.merge)

explots<-which(refl$row!=(-3) & refl$row < 161 & refl$range!=1 & refl$range!=26)

sr.raw<-refl[explots,]

dat.full.sr<-sr.raw[,5:ncol(sr.raw)]

vis.400.700<-rowMeans(dat.full.sr[sr.wave%in%c(400:700)])
nir.700.1000<-rowMeans(dat.full.sr[sr.wave%in%c(700:1000)])

dat.sr<-data.frame(cbind(ref.ts[explots,1:4], vis.400.700,nir.700.1000))

#Final cleanup
rm('refl.merge', 'refl.lw', 'refl.sw','ref.ts', 'dat.full.sr', 'sr.raw')

