# This script processes the all-day records from the 2020 miniplots/

source('ImportCS.R') #TOA5 importer



r13.raw<-importCSdata('allday/CR1000XSeries_1_Quantum_08_14_20_B3_R3.dat')
#('allday/CR1000XSeries_1_Quantum_08_19_20_B3_R13.dat') #Raw data

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

r13.ts<-breaktime(r13.raw$TIMESTAMP)
r13.ts$decdoy<-r13.ts$DOY+(r13.ts$dectime/24)
plot(r13.raw$PPF_above_Avg~r13.ts$decdoy)

#Remove times we don't want

#Find runthrough times####
path<-"allday/splits"
list.files(path)
spl<-read.csv(paste(path, "Allday_mini_5_3_B3.csv", sep='/'), header=FALSE)
times<-(as.POSIXlt(strptime(spl[,4],"%Y-%m-%d %H:%M:%S")))
plots<-spl[,1:3]; colnames(plots)<-c('row', 'range','io')
dectime<-breaktime(timestamp=times)$dectime
jd<-as.numeric(format(times, "%j"))
splits<-cbind(dectime,jd, plots)
#####

#Take out runthroughs
startrun<-splits[splits$dectime==min(splits$dectime),]; endrun<-splits[splits$dectime==max(splits$dectime),]
startrun.decdoy<-startrun$jd+(startrun$dectime/24)-(10/1440); endrun.decdoy<-endrun$jd+(endrun$dectime/24)+(10/1440) #add 10 min either side for fuckery

r13<-cbind(r13.ts, r13.raw)
r13<-r13[r13$decdoy<startrun.decdoy,]

#Take out first few min where I may be fucking about
newstart<-min(r13$decdoy)+(5/1440) #clip off 5 min
r13<-r13[r13$decdoy>newstart,]

#Take out dup hours (will add when I have dups)


#scale to above-canopy sensor
r13.dat<-r13[,12:17]/r13$PPF_above_Avg;r13.dat[r13$PPF_above_Avg<10,1:5]<-0
r13.ts<-r13[,1:7]
r13.preagg<-cbind(r13.ts, r13.dat)
r13.agg<-aggregate(. ~M+H, data=r13.preagg, mean, na.rm=TRUE)

plot(r13.agg$PPF5_Avg~r13.agg$dectime, ylim=c(0,1), xlim=c(9, 17), col=1)
points(r13.agg$PPF4_Avg~r13.agg$dectime, col=2)
points(r13.agg$PPF3_Avg~r13.agg$dectime, col=3)
points(r13.agg$PPF2_Avg~r13.agg$dectime, col=4)
points(r13.agg$PPF1_Avg~r13.agg$dectime, col=5)



# plot(r13.preagg$PPF5_Avg~r13.preagg$dectime, ylim=c(0,1), col=1)
# points(r13.preagg$PPF4_Avg~r13.preagg$dectime, col=2)
# points(r13.preagg$PPF3_Avg~r13.preagg$dectime, col=3)
# points(r13.preagg$PPF2_Avg~r13.preagg$dectime, col=4)
# points(r13.preagg$PPF1_Avg~r13.preagg$dectime, col=5)

#cumulative ppfs
r13.day<-r13[order(r13$dectime),]
plot(cumsum(r13.day$PPF_above_Avg)~r13.day$dectime, type='l', col='yellow')
lines(cumsum(r13.day$PPF1_Avg)~r13.day$dectime, col=5)
lines(cumsum(r13.day$PPF2_Avg)~r13.day$dectime, col=4)
lines(cumsum(r13.day$PPF3_Avg)~r13.day$dectime, col=3)
lines(cumsum(r13.day$PPF4_Avg)~r13.day$dectime, col=2)
lines(cumsum(r13.day$PPF5_Avg)~r13.day$dectime, col=1)

masterprof<-cbind(cumsum(r13.day$PPF_above_Avg),cumsum(r13.day$PPF5_Avg),cumsum(r13.day$PPF4_Avg),cumsum(r13.day$PPF3_Avg),cumsum(r13.day$PPF2_Avg),cumsum(r13.day$PPF1_Avg))[17108,]


plot(masterprof, type='l')

#make an example scaled plot
height<-125
#cart<-c(13,50,100,150,200,250)
masterprof[1:2]<-NA #Clip off too-high sensors
newheight<-c(125,100,50,30)

prof.scale<-masterprof/max(masterprof, na.rm=TRUE)
height.scale<-newheight/max(newheight)

par(mar=c(4,4,1,1))
plot(rev(prof.scale[3:6])~height.scale, type='l', lwd=2, ylab=("proportion full sun"), xlab="canopy depth")
lines(rev(c(1,0.45, 0.3,0))~height.scale, col='blue', lwd=2)
legend(0.7, 1, legend=c("measured at noon", "cumulative estimate"), col=c('blue', 'black'), lty=1)



#r3.raw<-importCSdata('allday/CR1000XSeries_1_Quantum_08_17_20_B3_R8.dat')
#r3.ts<-breaktime(r3.raw$TIMESTAMP)

