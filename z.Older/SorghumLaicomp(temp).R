#Reding in LAI for comparison between manual and automated
lai.man<-read.csv("LAI Energy Farm and Bondville - 2016-2020 - 2020.csv")
unique(lai.man$PLOT.ID)

sorg.lai<-lai.man[lai.man$PLOT.ID%in%c("SRG1", "SRG2", "SRG3", "SRE1", "SRE2", "SRE3", "SRW1", "SRW2", "SRW3"),]

pre<-c("SRG1", "SRG2", "SRG3");west<-c("SRG1", "SRG2", "SRG3","SRW1", "SRW2", "SRW3");east<-c("SRG1", "SRG2", "SRG3","SRE1", "SRE2", "SRE3")

e.ind<-which(sorg.lai$PLOT.ID%in%east); w.ind<-which(sorg.lai$PLOT.ID%in%west); p.ind<-which(sorg.lai$PLOT.ID%in%pre)

sorg.lai.share<-sorg.lai[, c(1,3,7,9:10)]
sorg.lai.share$condition<-rep("unspecified", nrow(sorg.lai.share))
sorg.lai.share$condition[e.ind]<-"Undisturbed"; sorg.lai.share$condition[w.ind]<-"Disturbed"; sorg.lai.share$condition[p.ind]<-"Pre-Flood"
sorg.lai.share$rep<-rep(0, nrow(sorg.lai.share)); sorg.lai.share$rep<-as.numeric(substr(sorg.lai.share$PLOT.ID, 4,5))
sorg.lai.share$PLOT.ID<-substr(sorg.lai.share$PLOT.ID, 1,3)

write.csv(sorg.lai.share, "Efarm_Sorghum_2020_Hand_LAI.csv", row.names=FALSE)

plot(sorg.lai$LAI[e.ind]~sorg.lai$Day.Number[e.ind], col='blue')
points(sorg.lai$LAI[w.ind]~sorg.lai$Day.Number[w.ind], col='red')
points(sorg.lai$LAI[p.ind]~sorg.lai$Day.Number[p.ind], col='black')

boxplot(data=sorg.lai[e.ind,], LAI~Day.Number, col='blue')
boxplot(data=sorg.lai[w.ind,], LAI~Day.Number, col='red', add=TRUE)
boxplot(data=sorg.lai[p.ind,], LAI~Day.Number, col='black', add=TRUE)

#Read in lidar cart data
lidar.raw<-read.csv("results 2020-11-02 120808.csv", as.is=TRUE)
lidar.raw$date<-substr(lidar.raw$file_name, 1, 10)
lidar.ts<-as.POSIXct(strptime(lidar.raw$date,format="%m_%d_%y"))
lidar.raw$doy<-as.numeric(format(lidar.ts, "%j"))
lidar.raw$row<-as.numeric(substr(lidar.raw$file_name, 12,12))
lidar.raw$range<-as.numeric(substr(lidar.raw$file_name, 14,14))
lidar.dat<-lidar.raw[,2:7]

library(esquisse)
esquisser()
library(ggplot2)
ggplot(lidar.dat[lidar.dat$doy>192,]) +
  aes(x = row, y = range, fill = lai) + #, colour = flood
  geom_tile(size = 1L) +
  scale_fill_viridis_c(option = "viridis") +
  #scale_color_viridis_c(option = "magma") +
  theme_minimal() +
  facet_wrap(vars(date))



lidar.dat$flood<-0
lidar.dat$flood[lidar.dat$range==1]<-1
lidar.dat$flood[lidar.dat$range==2&lidar.dat$row>1]<-1
lidar.dat$flood[lidar.dat$range==3&lidar.dat$row==6]<-1

lidar.dat.share<-lidar.dat; 
lidar.dat.share$flood[lidar.dat.share$flood==1]<-"Disturbed"; lidar.dat.share$flood[lidar.dat.share$flood==0]<-"Undisturbed"
colnames(lidar.dat.share)[5:7]<-c("S coordinate", "E coordinate", "condition")

write.csv(lidar.dat.share, "Efarm_Sorghum_2020_LiDAR_LAI.csv", row.names=FALSE)


lidar.w<-lidar.dat[lidar.dat$flood==1,]; lidar.e<-lidar.dat[lidar.dat$flood==0,]
boxplot(data=lidar.w, lai~doy, col='red', ylim=c(0, 10))
boxplot(data=lidar.e, lai~doy, add=TRUE, col='blue')

#Boxplots together (kinda noisy)
boxplot(data=sorg.lai[e.ind,], LAI~Day.Number, border='blue')
boxplot(data=sorg.lai[w.ind,], LAI~Day.Number, border='red', add=TRUE)
boxplot(data=sorg.lai[p.ind,], LAI~Day.Number, border='black', add=TRUE)

boxplot(data=lidar.w, lai~doy, col='red', ylim=c(0, 10))
boxplot(data=lidar.e, lai~doy, add=TRUE, col='blue')


#using the means
#LAI
manual.e<- aggregate(sorg.lai$LAI[e.ind], by=list(sorg.lai$Day.Number[e.ind]), FUN='mean')
manual.w<- aggregate(sorg.lai$LAI[w.ind], by=list(sorg.lai$Day.Number[w.ind]), FUN='mean')
auto.e<-aggregate(lidar.e$lai, by=list(lidar.e$doy), FUN='mean')
auto.w<-aggregate(lidar.w$lai, by=list(lidar.w$doy), FUN='mean')

plot(manual.e, pch=3, col='blue', type='l', lty=2, ylim=c(0,7), xlim=c(190, 250), lwd=2, main='LAI', ylab="LAI", xlab="Day of Year")
abline(v=c(195, 210, 233, 249), lty=3)
lines(manual.w, col='red',lty=2, lwd=2)
lines(auto.e, col='blue', lwd=2)
lines(auto.w, col='red', lwd=2)



legend(210, 2,legend=c("manual,  flood", "automated, flood", "manual, nonflood", "automated, nonflood"), 
       col=c("red","red","blue", "blue"), lty=c(2,1,2,1), lwd=2, cex=0.9, bty='n', text.font=2)

#height
manual.e<- aggregate(sorg.lai$Height..m.[e.ind], by=list(sorg.lai$Day.Number[e.ind]), FUN='mean')
manual.w<- aggregate(sorg.lai$Height..m.[w.ind], by=list(sorg.lai$Day.Number[w.ind]), FUN='mean')
auto.e<-aggregate(lidar.e$height, by=list(lidar.e$doy), FUN='mean')
auto.w<-aggregate(lidar.w$height, by=list(lidar.w$doy), FUN='mean')

plot(manual.e, pch=3, col='blue', type='l', lty=2, ylim=c(0,4), xlim=c(190, 250), lwd=2, main="Height(m)")
lines(manual.w, col='red',lty=2, lwd=2)
lines(auto.e, col='blue', lwd=2)
lines(auto.w, col='red', lwd=2)






