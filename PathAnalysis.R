#Run linear modeling before running this script

dat.raw<-kitsin.full #dataset of interesting variables

library(lavaan)
library(semPlot)

##Full model for reference####
model<-'
Yield~LAI+Height+Interception_efficiency+Lodging_Score+Elevation+Flood_Affected_1+Light_at_50+NIR_VIS_ratio
LAI~Flood_Affected_1+Elevation+Height
Height~Flood_Affected_1+Elevation
Interception_efficiency~LAI
Lodging_Score~Height
Proportion_Saturated_Sun~Height+LAI
NIR_VIS_ratio~Flood_Affected_1+Elevation

'
#####


#Model to mess with
model<-'
Yield~LAI+Height+Lodging_Score+Elevation+Proportion_Saturated_Sun+NIR_VIS_ratio
LAI~Flood_Affected_1+Height
Height~Flood_Affected_1
Lodging_Score~Height
Proportion_Saturated_Sun~Height+LAI
NIR_VIS_ratio~Flood_Affected_1


'
#NIR_VIS_ratio~Flood_Affected_1+Elevation
#Flood_Affected_1~Elevation


#View data distributions
par(mfrow=c(2,2))
for(i in 1:ncol(kitsin.nums)){  #Use numeric columns
  plot(density(kitsin.nums[,i], na.rm=TRUE), main=colnames(kitsin.nums[i]))
  ntest<-shapiro.test(kitsin.nums[,i])
  print(paste(colnames(kitsin.nums[i]), ":", ntest$p.value))
}

#Example of normalization cleaning up linearity; need to explore with other vars
library(bestNormalize)
par(mfrow=c(1,1))
choice<-bestNormalize(kitsin.nums$Proportion_Saturated_Sun, allow_orderNorm=FALSE); choice
plot(density(choice[[1]], na.rm=TRUE))

par(mfrow=c(2,2))
plot(lm(Yield~Proportion_Saturated_Sun, dat=kitsin.nums))
plot(lm(kitsin.nums$Yield~choice[[1]]))


#Loop testing linearity
par(mfrow=c(2,2))
for(i in 1:8){ #skips columns known to be really non-normal

voi<-kitsin.nums[,i]
for(j in 1:8){
if(i!=j & (i-1)>=j){ #does not plot variables against themselves or pairings already plotted
cross<-kitsin.nums[,j]
test<-lm(voi~cross)
plot(test, main=paste(colnames(kitsin.nums)[i], "by", colnames(kitsin.nums)[j]))
}
}
}


#test<-cfa(model, data=kitsin.full)
library(lavaan);library(semPlot)

#Normalize light 

kitsin.num.norm<-kitsin.nums

#Just normalize all of them?

choice<-bestNormalize(kitsin.num.norm$Light_at_50, allow_orderNorm=FALSE); choice
newlight<-choice[[1]]

#Separate high and low elevation
elev.mean<-605; #elev.mean<-mean(kitsin.full$Elevation, na.rm=TRUE)

elev.above.mean<-kitsin.full$Elevation-elev.mean; elev.above.mean[elev.above.mean<0]<-0
elev.below.mean<-kitsin.full$Elevation-elev.mean; elev.below.mean[elev.below.mean>0]<-0
elev.unflooded<-kitsin.full$Elevation; elev.unflooded[kitsin.full$Flood_Affected_1==1]<-NA


kitsin.num.norm$Light_at_50_norm<-newlight
kitsin.num.norm$Elevation_Above<-elev.above.mean
kitsin.num.norm$Elevation_Below<-elev.below.mean
kitsin.num.norm$Elevation_Unflood<-elev.unflooded
kitsin.num.norm1<-cbind(kitsin.num.norm, kitsin.dum)
kitsin.std.norm<-cbind(scale(kitsin.num.norm, center=TRUE), kitsin.dum)
kitsin.calm<-kitsin.std.norm[which(kitsin.nums$Lodging_Score==0&kitsin.std.norm$Flood_Affected_1==0),]

model<-'
Yield~Height+LAI+Lodging_Score+NIR_VIS_ratio+Proportion_Saturated_Sun+Elevation_Above+Flood_Affected_1
LAI~Flood_Affected_1+Height+Elevation_Above
Height~Flood_Affected_1+Elevation_Above
Proportion_Saturated_Sun~Height+LAI+Flood_Affected_1
NIR_VIS_ratio~Flood_Affected_1
Lodging_Score~Height
'

model.calm<-'
Yield~Height+LAI+NIR_VIS_ratio+Proportion_Saturated_Sun+Elevation_Above
LAI~Height+Elevation_Above
Height~Elevation_Above
Proportion_Saturated_Sun~Height+LAI
'

#problem: call in lavaan syntax; effects.sem does not work on lavaan output

test.std<-sem(model, data=kitsin.std.norm, estimator="MLM")
#test.std<-sem(model.calm, data=kitsin.calm, estimator="MLM")

summary(test.std, fit.measures = TRUE, standardized=F,rsquare=T)
semPaths(test.std, 'std', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5)


semPaths(test.std, nCharNodes=5, layout="circle")

thing<-effects(test.std)

#semPaths(test, 'std', layout='circle', residuals=FALSE, nCharNodes=5)

#Deal with flood being binary
#test.bin<-cfa(model, data=kitsin.std, ordered="Flood_Affected_1")
#semPaths(test.bin, 'std', layout="circle", residuals=FALSE, nCharNodes=5)


#Need a latent/combined variable for light capture