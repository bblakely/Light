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
dir.main<-getwd()
setwd('Ancillary')
info.raw<-read.csv('PlotInfo_2019.csv')
count.raw<-read.csv('StandCount_2019.csv')
heights.raw <- read.csv('LidarHeight_2019.csv', skip=2, stringsAsFactors = FALSE)
lai.raw<-read.csv('LidarLAI_2019.csv', skip=2, stringsAsFactors = FALSE)
yield.raw<-read.csv('2019_Yield_1.csv')#read.csv('Yield_2019_1.csv') #read.csv('Yield_2019.csv')
alt.raw<-read.csv('GPS_2019.csv')
lodge.raw<-read.csv("Lodging_2019.csv")
setwd(dir.main)

#Read in albedo dataset (will take a long time the first time)
if(!exists('vis.400.700')){source('Readin_SR.R')}
dat.sr->sr.raw
colnames(sr.raw)[colnames(sr.raw)=="dectime"]<-'dectime.sr' #Rename dectime column to prevent conflict with lp dectime column (and resulting unwanted renaming) when merging

#Read in and process flood dataset
flood.raw<-read.csv("Ancillary/FloodPlots_2019.csv")
flood.raw$Edge<-as.character(flood.raw$Edge); flood.raw$Edge[flood.raw$Edge=="y"|flood.raw$Edge=='n']<-1


#Unpack McGrath style dates and subset heights, LAI to date closest to LP measurement date####
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

##Merge in stand counts, plot ID's, heights, LAI's, etc. ####

#This made sense when I was merging just 2-3 things together but this is getting ridiculous...
light1<-merge(light, info.raw, by.x=c('row', 'range'),by.y=c('first_row','range'), sort=FALSE) #Add plot info
light2<-merge(light1, count.raw,by.x=c('row', 'range'),by.y=c('first_row','range'), sort=FALSE) #Add stand counts
light3<-merge(light2, heights.lp, by.x=c('row', 'range'), by.y=c('first_row', 'range'),all.x=TRUE, sort=FALSE) #Add heights
light4<-merge(light3, lai.lp, by.x=c('row', 'range'), by.y=c('first_row', 'range'),all.x=TRUE, sort=FALSE) #Add LAI

light5<-merge(light4, yield.raw, by.x=c('plot_id'), by.y=c('plot_id'),all.x=TRUE, sort=FALSE) #Add yield
light5<-light5[order(light5$exp.order),]

light6<-merge(light5, lodge.raw, by.x=c('row', 'range'), by.y=c('Row', 'Range'),all.x=TRUE, sort=FALSE) #Add lodging score
light7<-merge(light6, alt.raw, by.x=c('row', 'range'), by.y=c('row', 'range'),all.x=TRUE, sort=FALSE) #Add GPS
light8<-merge(light7, sr.raw, by.x=c('row', 'range'), by.y=c('row', 'range'),all.x=TRUE, sort=FALSE) #Add albedo

light9<-merge(light8, y=flood.raw, by.x=c('row', 'range'), by.y=c('RowStart', 'Range'), all.x=TRUE, sort=FALSE)#Add flood index
light9<-light9[order(light9$exp.order),];light9$Edge[is.na(light9$Edge)]<-0 #requires reordering, setting NAs to 0
#####

#Clear out junk columns; creates 'dat.lp' ####

light.full<-light9
colnames(light.full)

var.want<-c("row", "range", "dectime", "DOY.x", "H", "M","S",
            "PPF1_Avg", "PPF2_Avg", "PPF3_Avg", "PPF4_Avg", "PPF5_Avg","PPF_above_Avg",
            "noisy","ns","raw.order","exp.order","plot_id","genotype_name","block_id", "set_id",
            "row_density","height","lai", "above_ground_dry_yield","plot_id.x","x", "y", "z","Score", "vis.400.700", "nir.700.1000", "Edge")
  
  #Full variable list
  #c("row", "range", "dectime", "DOY.x", "H", "M","S","BattV_Min",
  #"PanelT", "PPF1_Avg", "PPF2_Avg", "PPF3_Avg", "PPF4_Avg", "PPF5_Avg","PPF_above_Avg",
  #"noisy","ns","raw.order","exp.order","year" , "site_id","plot_id" , "row_set", "genotype_name","block_id", "set_id",
  #"date.x","row_density","comment","date.y","height","YEAR.x", "DOY.y", "date","lai","YEAR.y","DOY")

cols<-which(colnames(light.full)%in%var.want); colnames(light.full)[cols] #Columns of data you want; excludes duplicate metadata, dates, etc.
dat.lp<-light.full[,cols]
dat.look<-dat.lp[,c(1:3, 15:32)]
#####
# 
# #Grab checkline plots; creates dat.check #####
# library(stringr)
# checkind<-str_detect(dat.lp$set_id, "CHK")
# dat.check<-dat.lp[str_detect(dat.lp$set_id, "CHK"),]
# #####



#Reassign top of canopy values

if(mod.toc=="TRUE"){
  
  rowtops<-aggregate(dat.lp$PPF_above_Avg, by=list(dat.lp$row), FUN=function(x) quantile(x, 0.99))
  
  col<-rep(c('red', 'blue'), nrow(rowtops)/2)
  xax<-aggregate(dat.lp$dectime, by=list(dat.lp$row), FUN='mean')$x
  
  #plot(rowtops$x~xax, col=col)
  
  rowtops<-aggregate(dat.lp$PPF5_Avg, by=list(dat.lp$row), FUN=function(x) quantile(x, 0.99))
  #points(rowtops$x~xax, col=col, pch='*')

  modtop<-smooth.spline(x=xax, y=rowtops$x)
  newtops<-predict(modtop,x=dat.lp$dectime);#lines(newtops)
  
  dat.lp.orig<-dat.lp
  dat.lp$PPF_above_Avg<-newtops$y
}


#Acknowledge flood zone

# floodlist<-read.csv("Ancillary/FloodPlots_2019.csv")
# dat.lp$order<-c(1:nrow(dat.lp))
# dat.flood<-data.frame(merge(x=dat.lp, y=floodlist, by.x=c('row', 'range'), by.y=c('RowStart', 'Range'), all.x=TRUE, sort=FALSE))
# dat.flood$Edge.<-as.character(dat.flood$Edge.); dat.flood$Edge.[dat.flood$Edge.=="y"|dat.flood$Edge.=='n']<-1
# dat.flood<-dat.flood[order(dat.flood$order),] #reorder to match dat.re and have metadata at the front where I can see it
# dat.flood$Edge.[is.na(dat.flood$Edge.)]<-0


#Grab checkline plots; creates dat.check #####
library(stringr)
checkind<-str_detect(dat.lp$set_id, "CHK")
dat.check<-dat.lp[str_detect(dat.lp$set_id, "CHK"),]
#####


#CLEANUP
rm('lp.raw','lp.ts','light1','light2','light3','light4', 'light5','light6', 'light7','light8','light9','light','plot.cl','splits','heights.lp','lai.lp', 'light.full', 'timestamp.col', 'split.dir', 'doy.of.interest', 'modtop','newtops', 'rowtops', 'dat.sr') #intermediate steps in processing
rm('count.raw', 'info.raw', 'heights.raw','lai.raw', 'yield.raw', 'lodge.raw', 'alt.raw', 'sr.raw', 'flood.raw') #Raw ancillary

rm('dat.lp.orig') #Comment this out if you want uncorrected top-of-canopy light levels
rm('plotmeans','refl')#Comment this out if you want border plots and spectral reflectances


