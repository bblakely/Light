lidar.raw<-read.csv("results 2020-11-02 120031.csv", as.is=TRUE)

lidar.ts<-as.POSIXct(strptime(lidar.raw$date,format="%m_%d_%y"))

lidar.raw$doy<-as.numeric(format(lidar.ts, "%j"))

par(mfrow=c(6,2))
for(i in 1:6){
boxplot(data=lidar.raw[lidar.raw$line==(i-1),],height~doy,main=paste("height, line",i-1), ylim=c(1,4.2),col=plotmeans$line[i]+1)
boxplot(data=lidar.raw[lidar.raw$line==(i-1),],lai~doy, main=paste("lai, line",i-1), ylim=c(0,7),col=plotmeans$line[i]+1)
}

par(mfrow=c(5,2))
doys<-unique(lidar.raw$doy)
for(i in 1:5){
  boxplot(data=lidar.raw[lidar.raw$doy==doys[i],],height~line,main=paste("height", doys[i]), ylim=c(1,4.2),col=c(1,6,4,5,2,3))
  boxplot(data=lidar.raw[lidar.raw$doy==doys[i],],lai~line, main=paste("lai", doys[i]), ylim=c(0,7),col=c(1,6,4,5,2,3))
}

