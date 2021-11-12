#Light profile plot splitter:

#This script reads in data from BB light cart and splits by plot
#Final product is a light profile for each plot, including border plots

#Many functions are created here that are used elsewhere


source('ImportCS.R') #TOA5 importer

#Read in light data
lp.raw.all.earlyseas<-importCSdata('CR1000XSeries_1_Quantum_2020.dat') #Raw data
lp.raw.all.lateaug<-importCSdata('CR1000XSeries_1_Quantum_09_15_20_retrieved_bkup.dat')
lp.raw.all.lateseas<-importCSdata('CR1000XSeries_1_Quantum_miniplot_9-15.dat')
lp.raw.all<-rbind(lp.raw.all.earlyseas, lp.raw.all.lateaug, lp.raw.all.lateseas)

timestamp.col<-lp.raw.all$TIMESTAMP #Where is the timestamp in the full dataset?

#Lidar data
lidar.raw<-read.csv("results 2020-11-02 120031.csv", as.is=TRUE)
lidar.ts<-as.POSIXct(strptime(lidar.raw$date,format="%m_%d_%y"))
lidar.raw$doy<-as.numeric(format(lidar.ts, "%j"))
lidar.dat<-lidar.raw[,5:9]

#End-of-season ancillary data
ancil.raw<-read.csv("miniplot_ancillary.csv", skip=1)
ancil.varnames<-c("count", "yield","width", "lodging", "yield", "line", "range")
ancil<-ancil.raw[,colnames(ancil.raw) %in% ancil.varnames]
ancil.ag<-aggregate(ancil, by=list(ancil$line), FUN='mean')[,2:6]


#Break down timestamp, clean up datasets
breaktime<-function(timestamp){
  Y<-as.numeric(format(timestamp, "%Y"))
  H<-as.numeric(format(timestamp, "%H"))
  M<-as.numeric(format(timestamp, "%M"))
  S<-as.numeric(format(timestamp, "%S"))
  DOY<-as.numeric(strftime(timestamp, "%j"))
  dectime<-H+(M/60)+(S/3600)
  ts<-data.frame(cbind(Y,DOY,H, M, S, dectime))
  return(ts)
}

lp.ts.all<-breaktime(timestamp.col)
clip.ind<-which(lp.ts.all$Y==2020&lp.ts.all$DOY>200)
lp.ts<-lp.ts.all[clip.ind,];lp.raw<-lp.raw.all[clip.ind,]

tsdir<-'LP_timestamps/Miniplots_2020' #Where the timestamp files are
files<-list.files(tsdir); files

#User-defined arguments
vars.of.interest<-c( "dectime","row","range","DOY","pass","line","ns","H","M","S","BattV_Min","PanelT","PPF1_Avg","PPF2_Avg","PPF3_Avg","PPF4_Avg","PPF5_Avg","PPF_above_Avg")
heights<-c(13,50,100,150,200,250) #Double-check this 
ppf.names<-c("PPF1_Avg","PPF2_Avg","PPF3_Avg","PPF4_Avg","PPF5_Avg","PPF_above_Avg")

#Main loop. Summarizes data for each date####
plotmeans.all<-data.frame()
for (q in 1:length(files)){
  
split.dir<-paste(tsdir,'/',files[q], sep='') #What is the name of the directory where the manual splits are stored?

#Do you want to replace measured top-of-canopy with modeled top-of-canopy?
mod.toc<-"TRUE"

#Read in files with manual splits and combine into a single df.
splitzip<-function(path){
list<-list.files(path)
splits<-data.frame()
for(l in 1:length(list)){
spl<-read.csv(paste(path,list[l], sep='/'), header=FALSE)
times<-(as.POSIXlt(strptime(spl[,4],"%Y-%m-%d %H:%M:%S")))
plots<-spl[,1:3]; colnames(plots)<-c('row', 'range','io')
dectime<-breaktime(timestamp=times)$dectime
jd<-as.numeric(format(times, "%j"))
splitnew<-cbind(dectime,jd, plots)
splits<- rbind(splits,splitnew);colnames
}
return(splits)
}

splits<-splitzip(split.dir); splits<-splits[!is.na(splits$row),]
doy.of.interest<-unique(splits$jd)
add.pass<-rep(c(1,2,3,4), each=44)
add.line<-c(rep(c(0,rep(c(5,3,4,1,2), 4), 0), each=2),rev(rep(c(0,rep(c(5,3,4,1,2), 4), 0), each=2)))

#Correct odd dataframes, add pass and line info
if(doy.of.interest==233){splits<-splits[c(1:32, 35:nrow(splits)),]} #Extra observartions on Aug 20, remove first one which was likely an error

if(nrow(splits)==176){
splits$pass<-add.pass
splits$line<-add.line
  }else{
    print("Nonstandard number of timestamp observations, check data. Processing anyway...")
    if(doy.of.interest==274){
      print("This is Sep 30th and needs some extra processing...")
      extra<-nrow(splits)-length(add.pass)
      splits$pass<-c(add.pass, rep(5, extra))
      splits$line<-c(rep(add.line,2), add.line[1:extra])
      splits<-splits[39:nrow(splits),]
      splits$pass[splits$pass==5]<-1
      }else{
        print("This is Aug 8")
        splits$pass<-add.pass[1:nrow(splits)]
        splits$line<-add.line[1:nrow(splits)]
      }
  }

#Merge splits and data, clip to useful times.
combine<-function(dat, ts, split, doy=unique(ts$DOY)){
bigdat.all<-cbind(ts, dat)
bigdat<-bigdat.all[bigdat.all$DOY%in%doy,]

plot.combine<-merge(split, bigdat, by='dectime', all=TRUE) #Note that this will sort the resulting df by dectime

both.str<-max(min(which(!is.na(plot.combine$row))), min(which(!is.na(plot.combine$TIMESTAMP))))
both.end<-min(max(which(!is.na(plot.combine$row))), max(which(!is.na(plot.combine$TIMESTAMP))))

plot.cl<-plot.combine[both.str:both.end,]


return(plot.cl)

}

plot.cl<-combine(dat=lp.raw, ts=lp.ts, split=splits, doy=doy.of.interest)


#Split plots - most of the work is done by this function
plotsplit=function(dat, vars=vars.of.interest, out='mean', noise.tol=200){
  
uniquerows<-unique(dat$pass)[!is.na(unique(dat$pass))]
uniqueranges<-unique(dat$range)[!is.na(unique(dat$range))]
numcol<-which(colnames(dat)%in%vars)

bigdf<-data.frame()
meandf<-data.frame(matrix(nrow=length(uniquerows)*length(uniqueranges), ncol=length(numcol)+1)); colnames(meandf)<-c(colnames(dat[numcol]), 'noisy')

count<-0

for (o in 1:length(uniquerows)){
  for(a in 1:length(uniqueranges)){
    
    count<-count+1
    loc<-which(dat$pass==uniquerows[o] & dat$range==uniqueranges[a])
   
    #All splits (i.e. button pushes) will have 2 identical records b/c the campbell logger logs twice a second
    
    if(length(loc)==4){ #4 lines mean one start push (2 identical recs) and one end push (also 2 identical), i.e. what you want.
      
      ind<-c(min(loc):max(loc))
      datsub<-dat[ind,]
      
      bigdf<-rbind(bigdf, datsub) 
      
      #flagging for noise
      ppf.cols<-which(vars%in%ppf.names)
      ranges<-apply(datsub[,numcol],2,FUN=function(x) range(x)[2]-range(x)[1])[ppf.cols]
      noiseflag<-length(which(ranges>noise.tol))
      
      meandf[count,]<-c(colMeans(datsub[,numcol], na.rm=TRUE), noiseflag)
      
    }else{
     
    #Give feedback about the problem   
    if(length(loc)==8){print(paste("pass", uniquerows[o], "range", uniqueranges[a], "is doubled"))}
    else if(length(loc)==0){print(paste("pass", uniquerows[o], "range", uniqueranges[a], "is missing"))}
    else{print(paste("pass", uniquerows[o], "range", uniqueranges[a], "is irregular;","There are", length(which(dat$pass==uniquerows[o] & dat$range==uniqueranges[a])), "records with this label"))}
    
      #If there's only one 'end'  find the closest start
      if(length(which(dat$io[loc]=='e'))==2){ #if you only have one end
        
        ends<-loc[which(dat$io[loc]=='e')] #loc at the end record to match
        tdiff<-abs(dat$dectime[loc]-mean(dat$dectime[loc[which(dat$io[loc]=='e')]]));tdiff[tdiff==0]<-NA #tdiff are the time differences; NaN out zeroes because those are the end times
        starts<-loc[which(tdiff==min(tdiff, na.rm=TRUE))] #which record is closest to the end times
        
        loc.rep<-c(starts, ends)
        
        print(paste("pass", uniquerows[o], "range", uniqueranges[a], "is resolved by matching single end record"))
        
      }
      
      #If there's only one start and multiple ends (probably rare), same procedure
      if(length(which(dat$io[loc]=='s'))==2){
        
        ends<-loc[which(dat$io[loc]=='s')] #loc at the start
        tdiff<-abs(dat$dectime[loc]-mean(dat$dectime[loc[which(dat$io[loc]=='s')]]));tdiff[tdiff==0]<-NA 
        starts<-loc[which(tdiff==min(tdiff, na.rm=TRUE))] 
        
        loc.rep<-c(starts, ends)
        
        print(paste("pass", uniquerows[o], "range", uniqueranges[a], "is resolved by matching single start record"))
              
      }
      
      #if there are multiples of both, take latest matched pair that has a reasonable time recorded (not written yet)
      
      
      #Now do the thing you do for normal records with the correct timestamps
      ind<-c(min(loc.rep):max(loc.rep))
      datsub<-dat[ind,]
      
      bigdf<-rbind(bigdf, datsub)
      
      #flagging for noise
      ranges<-apply(datsub[,numcol],2,FUN=function(x) range(x)[2]-range(x)[1])[ppf.cols]
      noiseflag<-length(which(ranges>noise.tol))
      
      meandf[count,]<-c(colMeans(datsub[,numcol], na.rm=TRUE), noiseflag)
      
    }#Closes processing for abnormal timestamps
    
  }#Closes range loop
}#Closes row loop

if(out=='mean'){return(meandf)}else{return(bigdf)}

}
plotmeans<-plotsplit(plot.cl); #plotfull<-plotsplit(plot.cl, out='all')

plotmeans.orig<-plotmeans #back up the original values

#Clean up shaded top-of-canopy values
above.sd<-sd(plotmeans$PPF_above_Avg)
max.toc<-mean(plotmeans$PPF_above_Avg)+(2*above.sd);min.toc<-mean(plotmeans$PPF_above_Avg)-(2*above.sd)

plotmeans$PPF_above_Avg[plotmeans$PPF_above_Avg<min.toc]<-NA; plotmeans$PPF_above_Avg[plotmeans$PPF_above_Avg>max.toc]<-NA
print(paste(length(which(is.na(plotmeans$PPF_above_Avg))), "top-of-canopy values removed due to likely shading"))
plotmeans$PPF_above_Avg[is.na(plotmeans$PPF_above_Avg)]<-mean(plotmeans$PPF_above_Avg, na.rm=TRUE)

#####

#Additional QC####
#North/south column
plotmeans$ns<-'n';
plotmeans$ns[plotmeans$pass%%2==0]<-'s'

#Add int efficiency
plotmeans$ie<-1-(plotmeans$PPF1_Avg/plotmeans$PPF_above_Avg)

#quality filter
#will have options to filter on noise, length of record
#####

#Plots####

plotdat<-plotmeans[,12:17]
par(mfrow=c(3,2))
scale<-TRUE
if(scale==TRUE){
ylim=c(0, (1.1*max(plotdat)))
boxplot(data=plotmeans, PPF1_Avg~line, ylim=ylim, main="PPF1", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF2_Avg~line, ylim=ylim, main="PPF2", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF3_Avg~line, ylim=ylim, main="PPF3", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF4_Avg~line, ylim=ylim, main="PPF4", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF5_Avg~line, ylim=ylim, main="PPF5", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF_above_Avg~line, ylim=ylim, main="PPF Above", col=c(1,6,4,5,2,3))
}else{
boxplot(data=plotmeans, PPF1_Avg~line,  main="PPF1", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF2_Avg~line, main="PPF2", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF3_Avg~line,  main="PPF3", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF4_Avg~line,  main="PPF4", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF5_Avg~line,  main="PPF5", col=c(1,6,4,5,2,3))
boxplot(data=plotmeans, PPF_above_Avg~line, main="PPF Above", col=c(1,6,4,5,2,3))
}

#Basic plot to check out a sample of profiles
par(mfrow=c(1,1), mar=c(4,4,1,1))
heights<-heights

par(mfrow=c(3,2))
for (i in (1:6)){

  plot((as.numeric(plotdat[1,])*100)~heights, col='white', xlim=c(0,250), ylim=c(-5,120), xlab="Measurement height (cm)", ylab="% total sunlight (unitless)", main=paste("doy", doy.of.interest, "line", i-1))
  for (j in 1:length(unique(plotmeans$pass))){
    plotset<-(plotdat[plotmeans$line==(i-1)&plotmeans$pass==j,])
    plotmeans[plotmeans$line==(i-1)&plotmeans$pass==j,]
    lty<-1;if(unique(plotmeans$ns[plotmeans$line==(i-1)&plotmeans$pass==j])=="s"){lty<-3}
      for(k in 1:4){
      if(doy.of.interest>260){maxlight<-max(plotmeans$PPF_above_Avg[plotmeans$pass==j])}else{maxlight<-max(plotset[k,])} #if late sept, where plants are taller than the cart, use the highest light recorded in the pass, probably from line 3 where the  cart is still taller than the veg
      lines((as.numeric(plotset[k,]/maxlight)*100)~heights, col=plotmeans$line[i]+1, lwd=2, lty=lty) }
  }
}


for(i in 1:6){
  subs<-plotmeans[plotmeans$line==(i-1),12:17]
  ns.lab<-plotmeans$ns[plotmeans$line==(i-1)]
  boxplot(subs[ns.lab=='s',], col=plotmeans$line[i]+1, main=paste("doy", doy.of.interest, "line", i-1, "south facing"), ylim=c(0, (1.1*max(subs))))
}

for(i in 1:6){
  subs<-plotmeans[plotmeans$line==(i-1),12:17]
  ns.lab<-plotmeans$ns[plotmeans$line==(i-1)]
  boxplot(subs[ns.lab=='n',], col=plotmeans$line[i]+1, main=paste("doy", doy.of.interest, "line", i-1, "north facing"), ylim=c(0, (1.1*max(subs))))
}

for(i in 1:6){
subs<-plotmeans[plotmeans$line==(i-1),12:17]
ns.lab<-plotmeans$ns[plotmeans$line==(i-1)]
boxplot(subs, col=plotmeans$line[i]+1, main=paste("doy", doy.of.interest, "line", i-1, "all"), ylim=c(0, (1.1*max(subs))))
}

#####

#Add LAI, height
lidar.days<-unique(lidar.dat$doy)
matchdate<-lidar.days[which(abs(lidar.days-doy.of.interest)==min(abs(lidar.days-doy.of.interest)))]
lidar.sub<-lidar.dat[lidar.dat$doy==matchdate,]
plotmeans<-merge(plotmeans, lidar.sub, sort=FALSE) #Add plot info

#Add ancillary end-of-year data:
plotmeans<-merge(plotmeans, ancil)

plotmeans.all<-rbind(plotmeans.all, plotmeans) #Stick next date onto a master df

}

dat.lp<-plotmeans.all; dat.lp$PPF5_Avg[which(dat.lp$DOY==218&dat.lp$ns=="n")]<-NA

#Add other ancillary info
ancil.raw<-read.csv("miniplot_ancillary.csv", skip=1)

#Summarize some stuff
par(mfrow=c(2,2), mar=c(4,3,3,1))

#dat.check.bplot<-dat.check;dat.check.bplot$set_id<-as.factor(friendlynames)
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

par(mfrow=c(2,2), mar=c(3,4,1,1))

subset<-which(dat.lp$doy==234)

dat.min.bplot<-plotmeans.all[subset,];dat.min.bplot$line<-as.factor(dat.min.bplot$line)

library(multcompView)
#Height
bplot<-boxplot(dat.min.bplot$height ~ dat.min.bplot$line , ylim=c(min(dat.min.bplot$height) , 1.1*max(dat.min.bplot$height)) , ylab="height" , main="", plot=FALSE)
heightm<-(aov(height~line, data=dat.min.bplot)); summary(heightm)
xd<-TukeyHSD(heightm);xd
letlab<-generate_label_df(xd, 1); colcode<-c("white", "purple", "red", "yellow", "dark blue", "black")
over <- 0.1*max(bplot$stats[nrow(bplot$stats),] )
boxplot(dat.min.bplot$height ~ dat.min.bplot$line , ylim=c(min(dat.min.bplot$height) , 1.1*max(dat.min.bplot$height)) , ylab="height" , main="", col=colcode)
text( c(1:nlevels(dat.min.bplot$line)) , bplot$stats[nrow(bplot$stats),]+over , letlab[,1])

#LAI
bplot<-boxplot(dat.min.bplot$lai ~ dat.min.bplot$line , ylim=c(min(dat.min.bplot$lai) , 1.1*max(dat.min.bplot$lai)) , ylab="lai" , main="", plot=FALSE)
laim<-(aov(lai~line, data=dat.min.bplot)); summary(laim)
xd<-TukeyHSD(laim);xd
letlab<-generate_label_df(xd, 1); colcode=c("red", "red", "blue","purple", "blue", "yellow")
over <- 0.1*max(bplot$stats[nrow(bplot$stats),] )
boxplot(dat.min.bplot$lai ~ dat.min.bplot$line , ylim=c(min(dat.min.bplot$lai) , 1.1*max(dat.min.bplot$lai)) , ylab="lai" , main="", col=colcode)
text( c(1:nlevels(dat.min.bplot$line)) , bplot$stats[nrow(bplot$stats),]+over , letlab[,1])

dat.min.bplot.ancil<-ancil.raw[ancil.raw$line!=0,];  dat.min.bplot.ancil$line<-as.factor(dat.min.bplot.ancil$line)

#Row density
bplot<-boxplot(dat.min.bplot.ancil$count ~ dat.min.bplot.ancil$line , ylim=c(min(dat.min.bplot.ancil$count) , 1.1*max(dat.min.bplot.ancil$count)) , ylab="count" , main="", plot=FALSE)
countm<-(aov(count~line, data=dat.min.bplot.ancil)); summary(countm)
xd<-TukeyHSD(countm);xd
letlab<-generate_label_df(xd, 1); colcode<-c("blue", "red","blue","red", "red")
over <- 0.1*max(bplot$stats[nrow(bplot$stats),], na.rm=TRUE)
boxplot(dat.min.bplot.ancil$count ~ dat.min.bplot.ancil$line , ylim=c(min(dat.min.bplot.ancil$count, na.rm=TRUE) , 1.1*max(dat.min.bplot.ancil$count, na.rm=TRUE)) , ylab="count" , main="", col=colcode)
text( c(1:nlevels(dat.min.bplot.ancil$line)) , bplot$stats[nrow(bplot$stats),]+over , letlab[,1])

#Yield
bplot<-boxplot(dat.min.bplot.ancil$yield ~ dat.min.bplot.ancil$line , ylim=c(min(dat.min.bplot.ancil$yield) , 1.1*max(dat.min.bplot.ancil$yield)) , ylab="yield" , main="", plot=FALSE)
yieldm<-(aov(yield~line, data=dat.min.bplot.ancil)); summary(yieldm)
xd<-TukeyHSD(yieldm);xd
letlab<-generate_label_df(xd, 1); colcode<-c("red", "red", "green", "purple","yellow")
over <- 0.1*max(bplot$stats[nrow(bplot$stats),], na.rm=TRUE)
boxplot(dat.min.bplot.ancil$yield ~ dat.min.bplot.ancil$line , ylim=c(min(dat.min.bplot.ancil$yield, na.rm=TRUE) , 1.1*max(dat.min.bplot.ancil$yield, na.rm=TRUE)) , ylab="yield" , main="", col=colcode)
text( c(1:nlevels(dat.min.bplot.ancil$line)) , bplot$stats[nrow(bplot$stats),]+over , letlab[,1])


# cols<-c("black","magenta","blue","cyan", "red", "green")
# boxplot(data=ancil.raw, count~as.factor(line), main='stem count', col=cols, xlab="Line")
# boxplot(data=ancil.raw, width~as.factor(line), main='stem width', col=cols, xlab="Line")
# boxplot(data=ancil.raw, lodging~as.factor(line), main='lodge score',col=cols, xlab="Line")
# boxplot(data=ancil.raw, yield~as.factor(line), main='yield',col=cols, xlab="Line")
# par(mfrow=c(1,1))
# boxplot(data=ancil.raw, yield~as.factor(line), main='yield')
# yield.aov<-(aov(data=ancil.raw, yield~as.factor(line)))
# TukeyHSD(yield.aov)
# 
# #More boxplots, of light stuff
# boxplot(data=plotmeans, lai~as.factor(line), main='lai', col=cols, xlab="Line")
# lai.aov<-(aov(data=plotmeans, lai~as.factor(line)))
# TukeyHSD(lai.aov)
# 
# boxplot(data=plotmeans, height~as.factor(line), main='height', col=cols, xlab="Line")
# height.aov<-(aov(data=plotmeans, height~as.factor(line)))
# TukeyHSD(height.aov)
# 
# boxplot(data=plotmeans.all[plotmeans.all$doy==234,], ie~as.factor(line), main='ie', col=cols, xlab="Line")
# ie.aov<-(aov(data=plotmeans.all[plotmeans.all$doy==234,], ie~as.factor(line))); summary(ie.aov)
# TukeyHSD(ie.aov)


#Pull together all extra data, one row per line

ancil.varnames<-c("count", "yield","width", "lodging", "yield", "line", "range")
ancil<-ancil.raw[,colnames(ancil.raw) %in% ancil.varnames]
ancil.ag<-aggregate(ancil, by=list(ancil$line), FUN='mean')[,2:6]

doys<-unique(lidar.dat$doy)

for(i in 1:length(doys)){
lai.day<-lidar.dat[lidar.dat$doy==doys[i],]
lai.day.ag<-aggregate(lai.day, by=list(lai.day$line), FUN='mean')[3:5]
colnames(lai.day.ag)[2:3]<-paste(colnames(lai.day.ag)[2:3], as.character(doys[i]), sep="_")
if(i==1){
  lai.days<-lai.day.ag}else{
  lai.days<-cbind(lai.days,lai.day.ag[2:3])
}
}
plot.summs<-merge(ancil.ag, lai.days)[2:6,]



# #Look at leaf area
# start.leafa<-which(colnames(ancil.raw)=="part_leafarea_L0")
# ancil.leafa<-ancil.raw[which(ancil.raw$line%in%c(3,4)),c(2:3, start.leafa:(start.leafa+6))]
# 
# leafa.dat<-ancil.leafa[3:(ncol(ancil.leafa)-1)]
# x<-c(0, 0.5, 1, 1.5, 2.0,4.0 );y<-c(0, 0.5, 1, 1.5, 2.0,3.0)
# plot(cumsum(as.numeric(leafa.dat[2,]))~x, col='white',ylim=c(0, max(ancil.leafa$part_leafarea_sum)), ylab="cumulative leaf area", xlab="heights (0.5m intervals)")
# for(l in which(ancil.leafa$line==3)){
#   lines(cumsum(as.numeric(leafa.dat[l,]))~y, col='cyan')
# }
# for(l in which(ancil.leafa$line==4)){
#   lines(cumsum(as.numeric(leafa.dat[l,]))~x, col='red')
# }
# legend(3,4000, legend=c("line 3", "line 4"), col=c("cyan", "red"), lwd=1)
# 
# #look at ie over  time
# plot(aggregate(ie~doy, data=plotmeans.all[plotmeans.all$line==1,], FUN='mean'), ylim=c(0.7, 1), type='l')
# lines(aggregate(ie~doy, data=plotmeans.all[plotmeans.all$line==2,], FUN='mean'), ylim=c(0.7, 1), col='red')
# lines(aggregate(ie~doy, data=plotmeans.all[plotmeans.all$line==3,], FUN='mean'), ylim=c(0.7, 1), col='blue')
# lines(aggregate(ie~doy, data=plotmeans.all[plotmeans.all$line==4,], FUN='mean'), ylim=c(0.7, 1), col="orange")
# lines(aggregate(ie~doy, data=plotmeans.all[plotmeans.all$line==5,], FUN='mean'), ylim=c(0.7, 1), col='green')
# lines(aggregate(ie~doy, data=plotmeans.all[plotmeans.all$line==0,], FUN='mean'), ylim=c(0.7, 1), col='purple')

