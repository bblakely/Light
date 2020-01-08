#Ancillary info!

info.raw<-read.csv('PlotInfo_2019.csv')
count.raw<-read.csv('StandCount_2019.csv')


explots<-which(plotmeans.sr$row!=(-3) & plotmeans.sr$row < 161 & plotmeans.sr$range!=1 & plotmeans.sr$range!=26)

light.raw<-plotmeans.sr #[explots,]

#Remove edge plots; conveniently, outside data does not include them
light<-light.raw[which(light.raw$row %in% unique(count.raw$first_row)& light.raw$range %in% unique(count.raw$range)), ]


lightinfo<-merge(light, info.raw, by.x=c('row', 'range'),by.y=c('first_row','range'))
lightcountinfo<-merge(lightinfo, count.raw,by.x=c('row', 'range'),by.y=c('first_row','range'))

library(stringr)

#cols<-c(1:3,10:15,20,22:24,26);colnames(lightcountinfo)[cols] #Columns of data you want; excludes duplicate metadata, dates, etc.
dat.sr<-lightcountinfo #[,cols]

checkind<-str_detect(dat.sr$set_id, "CHK")
dat.check<-dat.sr[str_detect(dat.sr$set_id, "CHK"),]
