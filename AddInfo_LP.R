#Ancillary info!

source('LightProf.R')

info.raw<-read.csv('PlotInfo_2019.csv')
count.raw<-read.csv('StandCount_2019.csv')


explots<-which(plotmeans$row!=(-3) & plotmeans$row < 161 & plotmeans$range!=1 & plotmeans$range!=26)

light.raw<-plotmeans[explots,]

#Remove edge plots; conveniently, outside data does not include them
light<-light.raw[which(light.raw$row %in% unique(count.raw$first_row)& light.raw$range %in% unique(count.raw$range)), ]


lightinfo<-merge(light, info.raw, by.x=c('row', 'range'),by.y=c('first_row','range'))
lightcountinfo<-merge(lightinfo, count.raw,by.x=c('row', 'range'),by.y=c('first_row','range'))

library(stringr)

cols<-c(1:3,10:15,20,22:24,26);colnames(lightcountinfo)[cols] #Columns of data you want; excludes duplicate metadata, dates, etc.
dat.lp<-lightcountinfo[,cols]

checkind<-str_detect(dat.lp$set_id, "CHK")
dat.check<-dat.lp[str_detect(dat.lp$set_id, "CHK"),]

#CLEANUP
rm('lp.raw','lp.ts','lightinfo','lightcountinfo', 'light','plot.cl','splits', 'count.raw', 'info.raw')

####
# 
# 
# chk<-dat.check$PPF1_Avg/dat.check$PPF_above_Avg;exp<-dat$PPF1_Avg/dat.check$PPF_above_Avg
# 
# lp.cat<-as.character(dat.check$set_id)
# par(mfrow=c(1,2))
# for(i in 1:length(unique(lp.cat))){
#   
#   nums<-chk[which(lp.cat==unique(lp.cat)[i])]
#   ran<-exp[sample(1:length(exp),16)]
#   
#   hist(nums, main=paste(unique(lp.cat[i])), ylim=c(0,12),xlim=c(0,1),breaks=seq(from=min(nums), to=max(nums), length.out = 10))
#   
#   hist(ran, main='random',ylim=c(0,12),xlim=c(0,1), breaks=seq(from=min(ran), to=max(ran), length.out = 10))
#   
#   #print(paste(refl.cat[i],(t.test(nums, ran)[3])))
#   
# }
# 
# 
# par(mfrow=c(1,2))
# 
# lw.exp<-nir.700.1000[explots];lw.chk<-nir.700.1000[checkind]
# 
# refl.cat<-as.character(dat.check$set_id)
# 
# for(i in 1:length(unique(refl.cat))){
#   
#   nums<-lw.chk[which(refl.cat==unique(refl.cat)[i])]
#   ran<-lw.exp[sample(1:length(lw.exp),16)]
#   
#   hist(nums, main=paste("LW ",unique(refl.cat[i])), xlim=c(0.2, 0.7), ylim=c(0,10), breaks=seq(from=min(nums), to=max(nums), length.out=10))
#   hist(ran, main='random', xlim=c(0.2, 0.7), ylim=c(0,10), breaks=seq(from=min(ran), to=max(ran), length.out=10))
#   
#   print(paste(refl.cat[i],(t.test(nums, ran)[3])))
# }
# 
