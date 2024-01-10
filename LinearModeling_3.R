
if (!exists('dat.print')){source('Normalize_LP.R')}
combo.lp<-cbind(dat.lp, dat.rel)
dat.lp$above_ground_dry_yield[dat.lp$above_ground_dry_yield>5]<-NA

gaussify<-TRUE #Set to true if you want non-normal predictors to be normalized.
#Changes results moderatelyt

#Height and yield####

library(ggplot2) 

ggplot(dat.lp) +
  aes(x = row, y = range, fill = above_ground_dry_yield) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "RdGy") +
  theme_minimal()



ggplot(dat.lp) +
  aes(x = lai, y = above_ground_dry_yield, colour = height) +
  geom_point(size = 1.58) +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

#####

#Create 'pcts' and 'ei', originally from phenome plots
#calculate pcts
extract.canval<-function(dat.h, dat.l, pct=0.5, res=20){
  holder<-rep(-1, nrow(dat.l))
  
  for(i in 1:nrow(dat.l)){
    
    heightstr<-approx(as.numeric(dat.h[i,]), n=res)$y
    datstr<-approx(as.numeric(dat.l[i,]), n=res)$y
    
    ind<-which(abs(heightstr-pct)==min(abs(heightstr-pct)))
    at.pct<-datstr[ind]
    
    holder[i]<-at.pct
  }
  
  return(holder)
}

pcts<-extract.canval(dat.h=height.rel, dat.l=dat.rel, res=100)

#calculate interception efficiency (does not account for reflectance)
ei<-1-(combo.lp$PPF1_Avg/combo.lp$PPF_above_Avg)
ei[combo.lp$PPF_above_Avg<combo.lp$PPF5_Avg]<-(1-(combo.lp$PPF1_Avg/combo.lp$PPF5_Avg))[combo.lp$PPF_above_Avg<combo.lp$PPF5_Avg]

#calculate absorbance (does account for reflectance)

ai<-1-(combo.lp$PPF1_Avg/(combo.lp$PPF_above_Avg*(1-combo.lp$vis.400.700))) #Consider including NIR

plot(ei~ai);lm(ei~ai)$coefficients



#####



##Linear modeling####
par(mfrow=c(1,1))

kitsin<-cbind(dat.lp, pcts, doesitfit, propsat,coefs, ai, dat.lp$nir.700.1000/dat.lp$vis.400.700)

#colnames(kitsin.og)[c(22:26,29:30, 31:38)]<-c("Row_Stem_Density", "Height", "LAI", "Yield", "Lodging_Score", "Elevation", "VIS_Reflectance","NIR_Reflectance", "Flood_Affected", "Light_at_50","Fit_Type","Proportion_Saturated_Sun","Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")
colnames(kitsin)[c(22:26,29:34,41:46)]<-c("Row_Stem_Density", "Height", "LAI", "Yield", "Lodging_Score", "Elevation", "VIS_Reflectance","NIR_Reflectance", "NDVI","Flood_Affected", "Lodging_Score2", "Light_at_50","Fit_Type","Proportion_Saturated_Sun","Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")

#NANs from mismatch between SVQ plots and our plots fixed here
kitsin$Elevation[is.na(kitsin$Elevation)]<-mean(kitsin$Elevation, na.rm=TRUE)
kitsin$Lodging_Score2[is.na(kitsin$Lodging_Score2)]<-0
#Give this better names

kitsin.og<-kitsin

#Transformations

library(bestNormalize)

hist(kitsin$Light_at_50)
bestNormalize(kitsin$Light_at_50, allow_orderNorm = FALSE)
hist(boxcox(kitsin$Light_at_50)$x.t)
Light_at_50<-boxcox(kitsin$Light_at_50)$x.t

hist(kitsin$Proportion_Saturated_Sun)
bestNormalize(kitsin$Proportion_Saturated_Sun, allow_orderNorm = FALSE)
hist(boxcox(kitsin$Proportion_Saturated_Sun)$x.t)
Proportion_Saturated_Sun<-boxcox(kitsin$Proportion_Saturated_Sun)$x.t

hist(kitsin$Curvefit_Steepness)
bestNormalize(kitsin$Curvefit_Steepness, allow_orderNorm = FALSE)
hist(yeojohnson(kitsin$Curvefit_Steepness)$x.t)
Curvefit_Steepness<-yeojohnson(kitsin$Curvefit_Steepness)$x.t

hist(kitsin$Interception_efficiency)
bestNormalize(kitsin$Interception_efficiency)#, allow_orderNorm = FALSE)
hist(orderNorm(kitsin$Interception_efficiency)$x.t)
Interception_efficiency<-orderNorm(kitsin$Interception_efficiency)$x.t

kitsin.gs<-kitsin
kitsin.gs$Proportion_Saturated_Sun<-Proportion_Saturated_Sun
kitsin.gs$Light_at_50<-Light_at_50; kitsin.gs$Curvefit_Steepness<-Curvefit_Steepness
kitsin.gs$Interception_efficiency<-Interception_efficiency

#Examine light prof variables normalized
par(mfrow=c(2,2))

plot(kitsin$Curvefit_Steepness~kitsin$Light_at_50)
plot(kitsin$Curvefit_Steepness~kitsin$Proportion_Saturated_Sun)
plot(kitsin$Light_at_50~kitsin$Proportion_Saturated_Sun)

par(mfrow=c(1,1))

if(gaussify==TRUE){kitsin<-kitsin.gs}

library(fastDummies)


#Generate datasets for modeling
kitsin.varwant<-c("Row_Stem_Density","Height", "LAI" ,"Yield", "Lodging_Score",
                  "Elevation", "VIS_Reflectance","NIR_Reflectance","NDVI","Flood_Affected","Lodging_Score2",
                  "DSM_p", "GC_p","NDVI_p","DSM_slp", "GC_slp", "NDVI_slp",
                  "Light_at_50", "Fit_Type","Proportion_Saturated_Sun", "Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")

kitsin.want<-kitsin[,colnames(kitsin)%in%kitsin.varwant];#[,c(22:26,29:46)];

kitsin.dum<-dummy_cols(kitsin.want)[,24:27]

kitsin.numnames<-c("Row_Stem_Density","Height", "LAI" ,"Yield", "Lodging_Score",
                   "Elevation", "VIS_Reflectance","NIR_Reflectance","NDVI","Lodging_Score2",
                   "DSM_p", "GC_p","NDVI_p","DSM_slp", "GC_slp", "NDVI_slp",
                   "Light_at_50", "Proportion_Saturated_Sun", "Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")

kitsin.nums<-kitsin.want[,colnames(kitsin.want)%in%kitsin.numnames]
kitsin.full<-cbind(kitsin.want, kitsin.dum)

kitsin.nums[,5]<-as.numeric(kitsin.nums[,5]); #kitsin.nums[,10]<-as.numeric(kitsin.nums[,10])
kitsin.std<-cbind(scale(kitsin.nums, center=TRUE), kitsin.dum)


#Dataset with checklines only
checkind<-str_detect(kitsin$set_id, "CHK") #row indices of checklines
kitsin.chk<-kitsin[checkind,]
kitsin.nums.chk<-kitsin.nums[checkind, ]
kitsin.std.chk<-kitsin.std[checkind,]

#Explore best versions of NDVI/height/lodging
# par(mfrow=c(1,2),mar=c(4,4,3,1))
# 
# kitsin.bk<-kitsin
# 
# kitsin$Elevation<-kitsin$Elevation-median(kitsin$Elevation)
# kitsin$Elevation[kitsin$Elevation<0]<-0
# 
# kitsin.na<-na.omit(kitsin); kitsin<-kitsin.na
# 
# lab.y<-3.5
# 
# plot(Yield~NDVI, data=kitsin, main="original")
# text(x=mean(kitsin$NDVI, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~NDVI, data=kitsin))$adj.r.squared,3)))
# plot(Yield~NDVI_p, data=kitsin , main="satellite")
# text(x=mean(kitsin$NDVI_p, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~NDVI_p, data=kitsin))$adj.r.squared,3)))
# 
# plot(Yield~Height, data=kitsin, main="original")
# text(x=mean(kitsin$Height, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~Height, data=kitsin))$adj.r.squared,3)))
# plot(Yield~DSM_p, data=kitsin , main="satellite")
# text(x=mean(kitsin$DSM_p, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~DSM_p, data=kitsin))$adj.r.squared,3)))
# 
# plot(Yield~Lodging_Score, data=kitsin, main="original")
# text(x=mean(kitsin$Lodging_Score, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~Lodging_Score, data=kitsin))$adj.r.squared,3)))
# plot(Yield~Lodging_Score2, data=kitsin , main="satellite")
# text(x=mean(kitsin$Lodging_Score2, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~Lodging_Score2, data=kitsin))$adj.r.squared,3)))
# 
# plot(Yield~LAI, data=kitsin, main="original")
# text(x=mean(kitsin$LAI, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~LAI, data=kitsin))$adj.r.squared,3)))
# plot(Yield~GC_p, data=kitsin , main="satellite")
# text(x=mean(kitsin$GC_p, na.rm=TRUE), y=lab.y, labels=paste("r2=", round(summary(lm(Yield~GC_p, data=kitsin))$adj.r.squared,3)))


#Test whether inst. growth rates are better related to light

par(mfrow=c(1,2), mar=c(4,4,1,1))

plot(Yield~DSM_slp, data=kitsin, xlab="rate of height growth (DSM_slp)") #nah, negative also
plot(Proportion_Saturated_Sun~DSM_slp, data=kitsin, xlab="rate of height growth (DSM_slp)", ylab="light penetration") #nah, negative also

plot(Yield~GC_slp, data=kitsin, xlab="rate of green coverage increase (GC_slp)") #nah, negative also
plot(Proportion_Saturated_Sun~GC_slp, data=kitsin,  xlab="rate of green coverage increase (GC_slp)", ylab="light penetration") #nah, negative also

plot(Yield~NDVI_slp, data=kitsin, xlab="rate of NDVI increase (NDVI_slp)") #nah, negative also
plot(Proportion_Saturated_Sun~NDVI_slp, data=kitsin,  xlab="rate of NDVI increase (NDVI_slp)", ylab="light penetration") #nah, negative also

#If plants are still closing canopy (i.e. GC increasing) by july
#They're behind.

#Linear models, full dataset#### 
library(jtools, broom)#, ggstance)

#sinkmodel.sat<-lm(Yield~DSM_p+NDVI_p+LAI+Proportion_Saturated_Sun+Light_at_50+Interception_efficiency+Row_Stem_Density+Flood_Affected+Lodging_Score+Fit_Type+Curvefit_Steepness+VIS_Reflectance+Elevation, data=na.omit(subset(kitsin)))
#sinkmodel.rate<-lm(Yield~DSM_p+GC_slp+DSM_slp+NDVI_p+LAI+Proportion_Saturated_Sun+Light_at_50+Interception_efficiency+Row_Stem_Density+Flood_Affected+Lodging_Score+Fit_Type+Curvefit_Steepness+VIS_Reflectance+Elevation, data=na.omit(subset(kitsin)))
sinkmodel.og<-lm(Yield~Height+LAI+Curvefit_Steepness+Proportion_Saturated_Sun+Light_at_50+Interception_efficiency+NDVI+Row_Stem_Density+Flood_Affected+Lodging_Score+Fit_Type+Elevation, data=na.omit(subset(kitsin)))
#sinkmodel<-lm(Yield~Height+NDVI_p+LAI+Proportion_Saturated_Sun+Light_at_50+Interception_efficiency+Row_Stem_Density+Flood_Affected+Lodging_Score+Fit_Type+Curvefit_Steepness+VIS_Reflectance+Elevation, data=na.omit(subset(kitsin)))


summary(sinkmodel.og)
#summ(sinkmodel)
plot_summs(sinkmodel.og, scale=TRUE, colors="forest green")#colors = "#7B883F")

step(sinkmodel.og)
selectmodel<-step(sinkmodel.og)

summary(selectmodel)
summ(selectmodel)
plot_summs(selectmodel, scale=TRUE, colors="forest green")#colors = "#7B883F")


par(mar=c(4,4,4,1))

library(dplyr)
library(ggplot2)


# sinkmodel2<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected_1+Flood_Affected_0+Lodging_Score+Elevation+VIS_Reflectance+NIR_Reflectance+Light_at_50+Fit_Type_E+Fit_Type_L+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.std)) #+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness [currently problems with those columns]
# summary(sinkmodel2)
# summ(sinkmodel2)


#####

#Linear models, checklines

kitsin.chk.nona<-na.omit(kitsin.chk) #not standardized

kitsin.chk.std.nona<-na.omit(cbind(kitsin.std.chk, kitsin.chk$set_id))#standardized
colnames(kitsin.chk.std.nona)[ncol(kitsin.chk.std.nona)]<- "set_id" #label tacked-on set_id (should do better later)


#non-standardized
sinkmodel.chk<-(lm(Yield~set_id+Row_Stem_Density+Height+LAI+Flood_Affected+Lodging_Score+Elevation+NDVI+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Fit_Type+Curvefit_Steepness, data=kitsin.chk.nona)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(sinkmodel.chk)
summ(sinkmodel.chk)
plot_summs(sinkmodel.chk, scale=TRUE, colors="forest green")#colors = "#7B883F")

selectmod<-step(sinkmodel.chk)

#selectmodel.chk<-(lm(formula = Yield ~ set_id + Height + LAI + Lodging_Score + 
                       #Proportion_Saturated_Sun + Flood_Affected,data = kitsin.chk.nona))

summary(selectmod)
summ(selectmod)
plot_summs(selectmod, scale=TRUE, colors="forest green")#colors = "#7B883F")

#standardized, 

sinkmodel.chk.std<-(lm(Yield~set_id+Row_Stem_Density+Height+LAI+Flood_Affected_1+Lodging_Score+Elevation+NDVI+Interception_efficiency+Light_at_50+
                         Proportion_Saturated_Sun+Fit_Type_L+Curvefit_Steepness, data=kitsin.chk.std.nona)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(sinkmodel.chk.std)
summ(sinkmodel.chk.std)
plot_summs(sinkmodel.chk.std, scale=TRUE, colors="forest green")#colors = "#7B883F")

selectmod<-step(sinkmodel.chk.std)


summary(selectmod)
summ(selectmod)
plot_summs(selectmod, scale=TRUE, colors="forest green")#colors = "#7B883F")


#Run for individual lines (doesn't show much, usually just if some plots were flooded)
kitsin.chk.std.nona<-na.omit(kitsin.std.chk)
kitsin.chk.nona<-na.omit(kitsin.chk)

line<-"CHK-C"

# sinkmodel.chk.ln<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected+Lodging_Score+Elevation+NDVI+
#                      Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Fit_Type+Curvefit_Steepness, 
#                    data=kitsin.chk.nona[kitsin.chk.nona$set_id==line,]))

sinkmodel.chk.ln.std<-(lm(Yield~Height+LAI+Flood_Affected_1+Lodging_Score+Elevation+NDVI+
                      Interception_efficiency+Row_Stem_Density+Light_at_50+Proportion_Saturated_Sun+Fit_Type_L+Curvefit_Steepness, 
                    data=kitsin.chk.std.nona[kitsin.chk.nona$set_id==line,]))

summ(sinkmodel.chk.ln.std)
#plot_summs(sinkmodel.chk.ln, scale=TRUE, colors="forest green")#colors = "#7B883F")
plot_summs(sinkmodel.chk.ln.std)

linemod<-step(sinkmodel.chk.ln.std)
plot_summs(linemod$call, scale=TRUE, colors="forest green")
summ(linemod)

#####

#####
#ggplots of variability####


library(ggplot2)


ggplot(kitsin) +
 aes(x = row, y = range, fill = NDVI) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Purples", 
 direction = 1) +
 theme_minimal()+
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        plot.title=element_text(size=26), axis.title=element_text(size=18,face="bold"), legend.title = element_text(size=20))+
  labs(fill="NDVI")

dev.copy(png,'NDVI.png', width=730, height=465)
dev.off()


ggplot(kitsin) +
 aes(x = row, y = range, fill = Proportion_Saturated_Sun) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Oranges", 
 direction = -1) +
 theme_minimal()+
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        plot.title=element_text(size=26), axis.title=element_text(size=18,face="bold"), legend.title = element_text(size=20))+
  labs(fill="LP")

dev.copy(png,'PropSatSun.png', width=730, height=465)
dev.off()

ggplot(kitsin) +
 aes(x = row, y = range, fill = VIS_Reflectance) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Blues", 
 direction = -1) +
 theme_minimal()+
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        plot.title=element_text(size=26), axis.title=element_text(size=18,face="bold"), legend.title = element_text(size=20))+
  labs(fill="Alb.")

dev.copy(png,'Alb.png', width=730, height=465)
dev.off()

ggplot(kitsin) +
 aes(x = row, y = range, fill = Interception_efficiency) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Greens", 
 direction = 1) +
 theme_minimal()+
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        plot.title=element_text(size=26), axis.title=element_text(size=18,face="bold"), legend.title = element_text(size=20))+
  labs(fill="IE")

dev.copy(png,'inteff.png', width=730, height=465)
dev.off()


#####

library(corrplot);library(corrtable)

cordat<-cor(as.matrix(kitsin.nums),use="complete.obs") #had [kitsin$Interception_efficiency>0.90,] subsetting kitsin nums
stats<-cor.mtest(cordat)
thing<-corrplot(cordat, p.mat = stats$p,method='color',sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")

correlation_matrix(kitsin.nums, digits=2)
save_correlation_matrix(kitsin.nums, digits=2, filename = "pairwise.csv", use="upper")

cordat<-cor(as.matrix(kitsin.std),use="complete.obs")
stats<-cor.mtest(cordat)
corrplot(cordat, p.mat = stats$p,method='color',sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")
corrplot(cordat, p.mat = stats$p,method='number',pch=2,sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")


correlation_matrix(kitsin.std, digits=2)
save_correlation_matrix(kitsin.std, digits=2, filename = "pairwisestd.csv", use="upper")

#stratified correlations
library(corrplot);library(corrtable)
cordat<-cor(subset(data.frame(as.matrix(kitsin.nums))),use="complete.obs") #had [kitsin$Interception_efficiency>0.90,] subsetting kitsin nums
stats<-cor.mtest(cordat)
corrplot(cordat, p.mat = stats$p,method='color',sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")
look<-correlation_matrix(subset(data.frame(as.matrix(kitsin.nums))),use="complete.obs")
