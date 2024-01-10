#Run linear modeling before running this script

if(!exists("kitsin")){source('LinearModeling_3.R')}

library(lavaan)
library(semPlot)



#Separate high and low elevation
elev.mean<-605; #elev.mean<-mean(kitsin.full$Elevation, na.rm=TRUE)

elev.above.mean<-kitsin.full$Elevation-elev.mean; elev.above.mean[elev.above.mean<0]<-0
elev.below.mean<-kitsin.full$Elevation-elev.mean; elev.below.mean[elev.below.mean>0]<-0
elev.unflooded<-kitsin.full$Elevation; elev.unflooded[kitsin.full$Flood_Affected_1==1]<-NA

kitsin.num.norm<-kitsin.nums

#kitsin.num.norm$Light_at_50_norm<-newlight
kitsin.num.norm$Elevation_Above<-elev.above.mean
kitsin.num.norm$Elevation_Below<-elev.below.mean
#kitsin.num.norm$Elevation_Unflood<-elev.unflooded
kitsin.num.norm1<-cbind(kitsin.num.norm, kitsin.dum)
kitsin.std.norm<-cbind(scale(kitsin.num.norm, center=TRUE), kitsin.dum)
kitsin.calm<-kitsin.std.norm[which(kitsin.nums$Lodging_Score==0&kitsin.std.norm$Flood_Affected_1==0),]

kitsin.calm.ns<-kitsin[which(kitsin$Lodging_Score==0&kitsin$Flood_Affected==0),]

kitsin.std.norm.bk<-kitsin.std.norm; kitsin.calm.bk<-kitsin.calm

kitsin.std.norm<-na.omit(kitsin.std.norm)
kitsin.calm<-na.omit(kitsin.calm)


par(mfrow=c(1,1))

model.og<-'
Yield~Height+LAI+Lodging_Score+NDVI+Curvefit_Steepness+Elevation_Above+Flood_Affected_1
LAI~Flood_Affected_1+Height+Elevation_Above
Height~Flood_Affected_1+Elevation_Above
Curvefit_Steepness~Height+LAI+Flood_Affected_1
NDVI~Flood_Affected_1
Lodging_Score~Height
'

model.calm.og<-'
Yield~Height+LAI+NDVI+Proportion_Saturated_Sun
LAI~Height
Proportion_Saturated_Sun~Height+LAI

'


model.calm<-'
Yield~Height+LAI+NDVI+Interception_efficiency+Proportion_Saturated_Sun
LAI~Height
Interception_efficiency~LAI
Proportion_Saturated_Sun~Height+LAI+Interception_efficiency

'


#Model, dsm
model.inst<-'
DSM_slp~Height+LAI+NDVI+Interception_efficiency+Elevation_Above+Proportion_Saturated_Sun
LAI~Flood_Affected_1+Height+Elevation_Above
Height~Flood_Affected_1+Elevation_Above
Interception_efficiency~LAI+Flood_Affected_1
Proportion_Saturated_Sun~Interception_efficiency+Height+LAI+Flood_Affected_1
NDVI~Flood_Affected_1

'


#Model, new full
model<-'
Yield~Height+Lodging_Score+LAI+NDVI+Interception_efficiency+Elevation_Above+Proportion_Saturated_Sun
LAI~Flood_Affected_1+Height+Elevation_Above
Height~Flood_Affected_1+Elevation_Above
Lodging_Score~Height
Interception_efficiency~LAI+Flood_Affected_1
Proportion_Saturated_Sun~Interception_efficiency+Height+LAI+Flood_Affected_1
NDVI~Flood_Affected_1

'

#Can have elevation in or out; out very slightly better fit
#Can have flooding driving PSS or not, in slightly better fit


result.inst<-test.std<-sem(model.inst, data=kitsin.std.norm, estimator="MLM")
result.calm<-test.std<-sem(model.calm, data=kitsin.calm, estimator="MLM")

summary(test.std, fit.measures = TRUE, standardized=F,rsquare=T)
semPaths(test.std,'std', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5)

#print code-generated rate path model
semPaths(result.inst,'std', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5, edge.label.cex = 1.5, sizeMan=8)
dev.copy(png, width=800, height=550, "plots/FigS2_rate.png")
dev.off()

#print code-generated subset path model
semPaths(result.calm,'std', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5, edge.label.cex = 1.5, sizeMan=8)
dev.copy(png, width=800, height=550, "plots/FigS2_subset.png")
dev.off()


#semPaths(test.std, nCharNodes=5, layout="circle") #Makes simple line path diagram (no weights, etc.)

thing<-effects(test.std)

#semPaths(test, 'std', layout='circle', residuals=FALSE, nCharNodes=5)

#Deal with flood being binary
#test.bin<-cfa(model, data=kitsin.std, ordered="Flood_Affected_1")
#semPaths(test.bin, 'std', layout="circle", residuals=FALSE, nCharNodes=5)


#Need a latent/combined variable for light capture

#####
modelparam<-'
Yield~b1*Height+b2*LAI+b3*Lodging_Score2+b4*NDVI+b5*Proportion_Saturated_Sun+c1*Elevation_Above+c2*Flood_Affected_1
LAI~a3*Flood_Affected_1+d1*Height+a7*Elevation_Above
Height~a4*Flood_Affected_1+a6*Elevation_Above
Proportion_Saturated_Sun~d2*Height+d3*LAI+a2*Flood_Affected_1
NDVI~a1*Flood_Affected_1
Lodging_Score2~d4*Height

a4b1:= a4*b1
a4d2:= a4*d2
a4d1b2 := a4*d1*b2
a4d2b5 := a4*d2*b5
a4d4b3 := a4*d4*b3
a3b2 := a3*b2
a3d3:= a3*d3
a3d3b5 := a3*d3*b5
a1b4 := a1*b4
a2b5 := a2*b5
a6b1 := a6*b1
a6d1b2 := a6*d1*b2
a6d2b5 := a6*d2*b5
a6d4b3 := a6*d4*b3
a7b2 := a7*b2
a7d3b5 := a7*d3*b5
d4b3:= d4*b3

total := c1+c2+(a4*b1)+(a4*d1*b2)+(a4*d2*b5)+(a4*d4*b3)+(a3*b2)+(a3*d3*b5)+(a1*b4)+(a2*b5)+(a6*b1)+(a6*d1*b2)+(a6*d2*b5)+(a6*d4*b3)+(a7*b2)+(a7*d3*b5)


'
#####




modelparam2<-'
Yield~b1*Height+b2*LAI+b3*Lodging_Score+b4*NDVI+b5*Proportion_Saturated_Sun+b6*Interception_efficiency+c1*Elevation_Above
LAI~a3*Flood_Affected_1+d1*Height+a7*Elevation_Above
Height~a4*Flood_Affected_1+a6*Elevation_Above
Proportion_Saturated_Sun~d2*Height+d3*LAI+d5*Interception_efficiency+a2*Flood_Affected_1
NDVI~a1*Flood_Affected_1
Lodging_Score~d4*Height
Interception_efficiency~d6*LAI+a8*Flood_Affected_1

a4b1:= a4*b1
a4d2:= a4*d2
a4d1b2 := a4*d1*b2
a4d2b5 := a4*d2*b5
a4d4b3 := a4*d4*b3
a3b2 := a3*b2  
a3d3:= a3*d3
a3d3b5 := a3*d3*b5
a1b4 := a1*b4
a2b5 := a2*b5
a6b1 := a6*b1
a6d1b2 := a6*d1*b2
a6d2b5 := a6*d2*b5
a6d4b3 := a6*d4*b3
a7b2 := a7*b2
a7d3b5 := a7*d3*b5
d4b3:= d4*b3
a8b6:= a8*b6
a8d5b5:= a8*d5*b5
a3d6:= a3*d6
a3d6b6:= a3*d6*b6
d6d5:= d6*d5

total := c1+(a4*b1)+(a4*d1*b2)+(a4*d2*b5)+(a4*d4*b3)+(a3*b2)+(a3*d3*b5)+(a1*b4)+(a2*b5)+(a6*b1)+(a6*d1*b2)+(a6*d2*b5)+(a6*d4*b3)+(a7*b2)+(a7*d3*b5)


'


result<-sem(modelparam2, data=kitsin.std.norm)
est<-parameterestimates(result, standardized = TRUE, rsquare = TRUE)

semPaths(result, 'stdresu', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5)


#print code-generated subset path model
semPaths(result,'std', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5, edge.label.cex = 1.5, sizeMan=8)
dev.copy(png, width=800, height=550, "plots/FigS1_pathmodel.png")
dev.off()



stdresult<-standardizedsolution(result) #will give proper ci's



modelparam.calm<-'
Yield~c1*Height+b1*LAI+c2*NDVI+b2*Interception_efficiency+b3*Proportion_Saturated_Sun
LAI~a1*Height
Interception_efficiency~d2*LAI
Proportion_Saturated_Sun~a2*Height+d1*LAI+d3*Interception_efficiency

'

result.calm<-sem(modelparam.calm, data=kitsin.calm)
est.calm<-parameterestimates(result.calm, standardized = TRUE, rsquare = TRUE)
semPaths(result.calm, 'stdresu', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5)
stdresult.calm<-standardizedsolution(result.calm) #will give proper ci's


#Model, dsm
modelparam.inst<-'
DSM_slp~b1*Height+b2*LAI+b4*NDVI+b6*Interception_efficiency+c1*Elevation_Above+b5*Proportion_Saturated_Sun
LAI~a3*Flood_Affected_1+d1*Height+a7*Elevation_Above
Height~a4*Flood_Affected_1+a6*Elevation_Above
Interception_efficiency~d6*LAI+a8*Flood_Affected_1
Proportion_Saturated_Sun~d5*Interception_efficiency+d2*Height+d3*LAI+a2*Flood_Affected_1
NDVI~a1*Flood_Affected_1

'
result.inst<-sem(modelparam.inst, data=kitsin.std.norm)
est.inst<-parameterestimates(result.inst, standardized = TRUE, rsquare = TRUE)
semPaths(result.inst, 'stdresu', layout='circle',residuals=FALSE, exoCov=FALSE, nCharNodes=5)
stdresult.inst<-standardizedsolution(result.inst) #will give proper ci's





