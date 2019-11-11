#Ancillary info adder:

# This script merges the light profiles processed in 'LightProf' with ancillary data from outside sources
# 'Outside' data from "TERRA Validation Data" on drive ; https://drive.google.com/drive/folders/0BxmhCCbvrZdjMS02c2gyRlV0T0E)
# Creates 'dat.lp' and 'dat.check', the main df's used in further analyses


#Run LightProf to create DF of light prof info####

source('LightProf.R')

#record original order with border plots.
plotmeans$raw.order<-c(as.numeric(rownames(plotmeans)))

#####

#Read in ancillary datasets and format for merging####

info.raw<-read.csv('PlotInfo_2019.csv')
count.raw<-read.csv('StandCount_2019.csv')
heights.raw <- read.csv('LidarHeight_2019.csv', skip=2, stringsAsFactors = FALSE)
lai.raw<-read.csv('LidarLAI_2019.csv', skip=2, stringsAsFactors = FALSE)

#Subset heights, LAI to date closest to LP measurement date
subdate<-function(dat, doy, year=2019, datecol='date'){ #Function may not work with different date formats!
  dat.ts<-as.POSIXct(dat[,which(colnames(dat)==datecol)])
  dat$YEAR<-as.numeric(format(dat.ts, format= '%Y'))
  dat$DOY<-as.numeric(format(dat.ts, format= '%j'))
  rel<-dat[which(dat$DOY==doy & dat$YEAR==year),] #DOY 211 = July 30, closest date to July 31 light measurement day
    
  return(rel)
}
heights.lp<-subdate(heights.raw, doy=211) #doy argument is the date in the height or lai datasets closest to the date light was measured
lai.lp<-subdate(lai.raw, doy=211)

#####

#Exclude non-experimental plots####

explots<-which(plotmeans$row!=(-3) & plotmeans$row < 161 & plotmeans$range!=1 & plotmeans$range!=26)
light<-plotmeans[explots,]
light$exp.order<-c(1:nrow(light)) #Record thinned order before merges, which scramble df order if sort is not set to FALSE

#####

##Merge in stand counts, plot ID's, heights, LAI's####

light1<-merge(light, info.raw, by.x=c('row', 'range'),by.y=c('first_row','range'), sort=FALSE) #Add plot info
light2<-merge(light1, count.raw,by.x=c('row', 'range'),by.y=c('first_row','range'), sort=FALSE) #Add stand counts
light3<-merge(light2, heights.lp, by.x=c('row', 'range'), by.y=c('first_row', 'range'),all.x=TRUE, sort=FALSE) #Add heights
light4<-merge(light3, lai.lp, by.x=c('row', 'range'), by.y=c('first_row', 'range'),all.x=TRUE, sort=FALSE) #Add LAI

#####

#Clear out junk columns; creates 'dat.lp' ####

light.full<-light4
colnames(light.full)

var.want<-c("row", "range", "dectime", "DOY.x", "H", "M","S",
            "PPF1_Avg", "PPF2_Avg", "PPF3_Avg", "PPF4_Avg", "PPF5_Avg","PPF_above_Avg",
            "noisy","ns","raw.order","exp.order","plot_id","genotype_name","block_id", "set_id","row_density","height","lai")
  
  #Full variable list
  #c("row", "range", "dectime", "DOY.x", "H", "M","S","BattV_Min",
  #"PanelT", "PPF1_Avg", "PPF2_Avg", "PPF3_Avg", "PPF4_Avg", "PPF5_Avg","PPF_above_Avg",
  #"noisy","ns","raw.order","exp.order","year" , "site_id","plot_id" , "row_set", "genotype_name","block_id", "set_id",
  #"date.x","row_density","comment","date.y","height","YEAR.x", "DOY.y", "date","lai","YEAR.y","DOY")

cols<-which(colnames(light.full)%in%var.want); colnames(light.full)[cols] #Columns of data you want; excludes duplicate metadata, dates, etc.
dat.lp<-light.full[,cols]

#####

#Grab checkline plots; creates dat.check #####
library(stringr)
checkind<-str_detect(dat.lp$set_id, "CHK")
dat.check<-dat.lp[str_detect(dat.lp$set_id, "CHK"),]
#####

#CLEANUP
rm('lp.raw','lp.ts','light1','light2','light3','light4', 'light','plot.cl','splits','heights.lp','lai.lp', 'light.full') #intermediate steps in processing
rm('count.raw', 'info.raw', 'heights.raw','lai.raw') #Raw ancillary


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
