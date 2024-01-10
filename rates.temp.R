#Exploring SVQ growth rate (and lidar growth rate maybe) data

#Get data from the data folder, then move back to the main folder
wd<-getwd()
setwd("Ancillary")

rates.raw<-read.csv("UAV_FEATURES_SDP_2019.csv")
lidar.ht.raw<-read.csv("LidarHeight_2019.csv", skip=2)

setwd(wd)

#Features:
#DSM: Height I think?
#GC:"Green coverage (GC) is defined as the ratio of green versus total pixels in each plot."
#NDVI: I know what NDVI is
#NDRE:Normalized difference red edge index (NDRE for short) 
#WDRVI:Wide Dynamic Range Vegetation Index
#EXG: Excess greennes
#NGRDI:Normalized green-red difference index

#Dates - Planted May 31 2019 = DOY 151

#30 / DOY 181: June 30
#40 / DOY 191: July 10
#50 / DOY 201: July 20
#60 / DOY 211: July 30
#70 / DOY 221: August 9
#80 / DOY 231: August 19
#90 / DOY 241: August 29
#98 / DOY 249: Sep 6


#Measured various days, fit a spline, extracted features at these days

#Take a look at a few time courses:



par(mfrow=c(3,3), mar=c(4,4,1,1))
exline<-sample(unique(rates.raw$plot_id), 1)
exdat<-subset(rates.raw, plot_id==exline)

for(i in 14:28){
plot(exdat[,i]~as.numeric(exdat$Dates), ylab=colnames(exdat)[i], xlab="Days after planting")
abline(v=60, col='red', lty=2)
}

#DOY 211 / DAP 60 is my date. Extract:

rates.dat<-rates.raw[rates.raw$Dates==60,]


