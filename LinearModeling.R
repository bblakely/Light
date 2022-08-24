
if (!exists('dat.print')){source('Normalize_LP.R')}
combo.lp<-cbind(dat.lp, dat.rel)
dat.lp$above_ground_dry_yield[dat.lp$above_ground_dry_yield>5]<-NA

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

#Create 'pcts' and 'ei', originally from phenomeplots
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

ai<-1-(combo.lp$PPF1_Avg/(combo.lp$PPF_above_Avg*(1-combo.lp$vis.400.700)))

plot(ei~ai);lm(ei~ai)$coefficients



#####



##Linear modeling####
par(mfrow=c(1,1))

dat.lp.mod<-cbind(dat.lp, pcts, doesitfit, propsat,coefs, ei)

library(corrplot)

#Pairwise comparisons #####
# plot(dat.lp.mod$height~dat.lp.mod$above_ground_dry_yield)
# height.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~height)
# #plot(height.pw)
# 
# plot(dat.lp.mod$lai~dat.lp.mod$above_ground_dry_yield)
# lai.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~lai); summary(lai.pw)
# #plot(lai.pw)
# 
# plot(dat.lp.mod$ei~dat.lp.mod$above_ground_dry_yield)
# ei.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~ei); summary(ei.pw)
# plot(ei.pw)
# 
# plot(dat.lp.mod$z~dat.lp.mod$above_ground_dry_yield)
# z.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~z); summary(z.pw)
# plot(z.pw)
# 
# plot(dat.lp.mod$Score~dat.lp.mod$above_ground_dry_yield)
# Score.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~Score); summary(Score.pw)
# plot(Score.pw)
# 
# plot(dat.lp.mod$vis.400.700~dat.lp.mod$above_ground_dry_yield)
# vis.400.700.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~vis.400.700); summary(vis.400.700.pw)
# plot(vis.400.700.pw)
# 
# plot(dat.lp.mod$nir.700.1000~dat.lp.mod$above_ground_dry_yield)
# nir.700.1000.pw<-lm(data=dat.lp.mod,above_ground_dry_yield~nir.700.1000); summary(nir.700.1000.pw)
# plot(nir.700.1000.pw)
#####

#kitchen sink dat b
#need random effect on genotype?
#What else might explain variability?

summary(lm(above_ground_dry_yield~row_density+height+lai+Edge+z+Score, data=dat.lp))


kitsin<-cbind(dat.lp, pcts, doesitfit, propsat,coefs, ei, dat.lp$nir.700.1000/dat.lp$vis.400.700)

colnames(kitsin)[c(22:26,29:30, 31:38)]<-c("Row_Stem_Density", "Height", "LAI", "Yield", "Lodging_Score", "Elevation", "VIS_Reflectance","NIR_Reflectance", "Flood_Affected", "Light_at_50","Fit_Type","Proportion_Saturated_Sun","Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")
#Give this better names
library(jtools, broom)#, ggstance)
sinkmodel<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected+Lodging_Score+Elevation+NIR_VIS_ratio+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Fit_Type+Curvefit_Steepness, data=kitsin)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(sinkmodel)
summ(sinkmodel)
plot_summs(sinkmodel, scale=TRUE, colors="forest green")#colors = "#7B883F")

submodel<-(lm(Yield~Height+LAI+Row_Stem_Density+Flood_Affected+Lodging_Score+Elevation+NIR_VIS_ratio+Interception_efficiency+Row_Stem_Density, data=kitsin)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
plot_summs(submodel, scale=TRUE, colors="forest green")#colors = "#7B883F")
summary(submodel)
summ(submodel)

#step(kitsin.std)
step(sinkmodel)
# selectmodel<-lm(formula = Yield ~ Height + LAI + Lodging_Score + Elevation + 
#                   VIS_Reflectance + NIR_Reflectance + Interception_efficiency + 
#                   Light_at_50, data = kitsin)


selectmodel<-lm(formula = Yield ~ Height + LAI + Lodging_Score + Elevation + 
                                   NIR_VIS_ratio + Interception_efficiency+Proportion_Saturated_Sun,
                                   data = kitsin)
                  
summary(selectmodel)
summ(selectmodel)
plot_summs(selectmodel, scale=TRUE, colors="forest green")#colors = "#7B883F")


sinkmodel.d<-(lm(Yield~Height+LAI+Row_Stem_Density+Flood_Affected_1+Lodging_Score+Elevation+NIR_VIS_ratio+Interception_efficiency+Light_at_50+Fit_Type_E+Curvefit_Steepness, data=kitsin.std)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(sinkmodel.d)
summ(sinkmodel.d)
plot_summs(sinkmodel.d, scale=TRUE, colors="forest green")#colors = "#7B883F")

step(sinkmodel.d)
selectmodel.d<-lm(formula = Yield ~ Height + LAI + Lodging_Score + Elevation + 
     NIR_VIS_ratio + Interception_efficiency, data = kitsin.std)
summary(selectmodel.d)
summ(selectmodel.d)
plot_summs(selectmodel.d, scale=TRUE, colors="forest green")#colors = "#7B883F")

#Structure explains some 31%, environment

library(fastDummies)
kitsin.want<-kitsin[,c(22:26,29:30, 31:38)];
kitsin.dum<-dummy_cols(kitsin.want)[16:19]
kitsin.nums<-kitsin.want[,c(1:8, 10, 12:15)]
kitsin.full<-cbind(kitsin.want, kitsin.dum)
kitsin.std<-cbind(scale(kitsin.nums, center=TRUE), kitsin.dum)

par(mar=c(4,4,4,1))

library(dplyr)
library(ggplot2)

kitsin.nums %>%
 filter(LAI >= 1L & LAI <= 8L) %>%
 ggplot() +
 aes(x = Light_at_50, y = Yield, colour = LAI) +
 geom_point(size = 2L) +
 scale_color_distiller(palette = "RdYlBu") +
  geom_smooth(method=lm, colour='black')+
 theme_minimal()
# sinkmodel2<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected_1+Flood_Affected_0+Lodging_Score+Elevation+VIS_Reflectance+NIR_Reflectance+Light_at_50+Fit_Type_E+Fit_Type_L+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.std)) #+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness [currently problems with those columns]
# summary(sinkmodel2)
# summ(sinkmodel2)

ggplot(kitsin) +
  aes(x = Flood_Affected, y = Height) +
  geom_boxplot(fill = "#ffffff") +
  theme_minimal()

#ggplots of variability


library(ggplot2)


ggplot(kitsin) +
 aes(x = row, y = range, fill = NIR_VIS_ratio) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Purples", 
 direction = 1) +
 theme_minimal()

dev.copy(png,'NIRVIS.png', width=730, height=465)
dev.off()


ggplot(kitsin) +
 aes(x = row, y = range, fill = Proportion_Saturated_Sun) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Oranges", 
 direction = -1) +
 theme_minimal()

dev.copy(png,'PropSatSun.png', width=730, height=465)
dev.off()

ggplot(kitsin) +
 aes(x = row, y = range, fill = VIS_Reflectance) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Blues", 
 direction = -1) +
 theme_minimal()

dev.copy(png,'Alb.png', width=730, height=465)
dev.off()

ggplot(kitsin) +
 aes(x = row, y = range, fill = Interception_efficiency) +
 geom_tile(size = 1.5) +
 scale_fill_distiller(palette = "Greens", 
 direction = 1) +
 theme_minimal()

dev.copy(png,'inteff.png', width=730, height=465)
dev.off()


# #Model predicting lodging#####
# library(vioplot)
# 
# lodge.sinkmodel<-(lm(Lodging_Score~Height+LAI+Flood_Affected_1+Row_Stem_Density+Elevation+NIR_VIS_ratio+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.std)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
# summary(lodge.sinkmodel)
# step(lodge.sinkmodel)
# 
# 
# lodge.step<-lm(formula = Lodging_Score ~ Row_Stem_Density + Height + LAI + 
#                    Flood_Affected_1 + Elevation + NIR_VIS_ratio + Proportion_Saturated_Sun, 
#                  data = kitsin.std)
# 
# summary(lodge.step);plot_summs(lodge.step)
# 
# 
# lodge.sparse<-lm(formula = Lodging_Score ~ Row_Stem_Density + Height + Flood_Affected_1+ NIR_VIS_ratio, 
#                  data = kitsin.std)
# 
# summary(lodge.sparse);plot_summs(lodge.sparse)
# #plot(Height~Lodging_Score, dat=kitsin); abline(coefficients(lm(Height~Lodging_Score, dat=kitsin)))
# #plot(Lodging_Score~Height, dat=kitsin); abline(coefficients(lm(Lodging_Score~Height, dat=kitsin)))
# 
# lodge.ind<-rep(0, nrow(kitsin));lodge.ind[which(kitsin$Lodging_Score>=3)]<-1
# boxplot(kitsin$Height~lodge.ind)
# 
# lodge.agg<-aggregate(Height~Lodging_Score, data=kitsin, FUN='mean')
# plot(lodge.agg, ylim=c(quantile(kitsin$Height, c(0.25,0.75)))); abline(h=mean(kitsin$Height))
# 
# vioplot(Height~Lodging_Score, data=kitsin)
# 
# #let's play...
# 
# outcome<-lodge.ind #index of plots with moderate to severe lodging
# 
# #Stratify heights
# 
# splits<-c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5)
# splits2<-c(0.5,  1,  1.5,  2,  2.5)
# splits3<-seq(from=0, to=2.5, by=0.2)
# 
# test<-as.numeric(cut(kitsin$Height, splits))
# 
# resp<-agg<-aggregate(outcome, by=list(test), FUN='mean')$x #odds of moderate to severe lodging at each height
# risk<-aggregate(kitsin$Height, by=list(test), FUN='mean')$x #mean heights in each bin
# 
# 
# fit<-nls(resp ~ a*exp(k*risk), start=c(a=0.0001, k=5))
# #fit<-nls(rest ~ a*(x^z^risk), start=c(a=0.0001, x=4, z=2))
# coef<-coefficients(fit);coef
# yp<-coef[1]*exp(coef[2]*risk)
# 
# plot(resp~risk, ylab="chance of moderate to severe lodging", xlab='height at DOY 212')
# lines(yp~risk)
# 
# 
# vioplot(kitsin$Lodging_Score~test, names=paste(splits[1:8], '-', splits[2:9]),
#         xlab='height (binned)', ylab='lodging score')
# 
# #y<-0.0005*(4^2^risk)
# #lines(y~risk)
# 
# #####
# 
# #Models exploring flood recovery#####
# 
# #Show how yield affected by flooding
# 
# kitsin.flood<-na.omit(kitsin[kitsin$Flood_Affected==1,])
# kitsin.flood<-na.omit(kitsin.std[kitsin.std$Flood_Affected_1==1,])
# 
# floodsink<-(lm(Yield~Row_Stem_Density+Height+LAI+Lodging_Score+Elevation+NIR_VIS_ratio+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun, data=kitsin.flood)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
# 
# step(floodsink)
# 
# flood.select<-lm(Yield ~ Row_Stem_Density + Height + LAI + Lodging_Score + 
#   Light_at_50, data=kitsin.flood)
#   
#   plot_summs(flood.select)
# #####  
#   
# #General show of damage
# par(mfrow=c(1,3))
# vioplot(kitsin$Yield~kitsin$Flood_Affected, ylim=c(1, 4),names=c("no flooding","flooding"), ylab='Yield', xlab="flood status")
# vioplot(kitsin$Yield~lodge.ind, ylim=c(1, 4), names=c("none to mild", "moderate to severe"), xlab='lodging status', ylab="Yield")
#   
# # #With double-smashed plants out (makes little difference)
# # par(mfrow=c(1,2))
# # vioplot(kitsin$Yield~kitsin$Flood_Affected, subset=which(lodge.ind==0),ylim=c(1, 4),names=c("no flooding","flooding"), ylab='Yield', xlab="flood status")
# # vioplot(kitsin$Yield~lodge.ind, ylim=c(1, 4),subset=which(kitsin$Flood_Affected==0), names=c("none to mild", "moderate to severe"), xlab='lodging status', ylab="Yield")
# 
# 
# #Relationship between height and lodging
# plot(resp~risk, ylab="chance of moderate to severe lodging", xlab='height at DOY 212')
# lines(yp~risk)

######
# submodel<-function(lai.min, lai.max, height.min, height.max, variable) { ####
# #kitsin.subset<-kitsin.want[which(kitsin.want$LAI<lai.max & kitsin.want$LAI>lai.min & kitsin.want$Height>height.min & kitsin.want$Height<height.max), ]
# #kitsin.subset.num<-(kitsin.subset[,c(1:8, 10, 12:13)]);kitsin.subset.dum<-dummy_cols(kitsin.subset)[12:16]
# #kitsin.subset.std<-cbind(scale(kitsin.subset.num, center=TRUE), kitsin.subset.dum)
# 
# kitsin.subset.std<-kitsin.std[which(kitsin.want$LAI<lai.max & kitsin.want$LAI>lai.min & kitsin.want$Height>height.min & kitsin.want$Height<height.max),]
#   
# submodel2<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected_0+Lodging_Score+Elevation+VIS_Reflectance+NIR_Reflectance+Light_at_50+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.subset.std)) #+Fit_Type_E+Fit_Type_L+
# 
# ind<-which(names(submodel2$coefficients)==variable)
# print(lai.min); print(nrow(kitsin.subset.std));print(submodel2$coefficients[ind]); 
# pch<-1
# if(!is.na(summary(submodel2)$coef[,4][ind])){if(summary(submodel2)$coef[,4][ind]<0.05){pch<-19}}else(pch=3)
# 
# points(submodel2$coefficients[ind]~lai.min, pch=pch)
# summary(submodel2)
# 
# }

# plot(1:7, ylim=c(-1, 1), xlim=c(0, 10), col='white')
# for (i in c(4:16)){submodel(lai.min=(i*0.5),lai.max = (i+1)*0.5, height.min=1.5,height.max=2.5, variable="NIR_Reflectance")}
# colnames(kitsin.want)
# 
# summ(submodel2)
######


library(corrplot)
cordat<-cor(as.matrix(kitsin.nums[kitsin$Interception_efficiency>0.90,]),use="complete.obs")
stats<-cor.mtest(cordat)
corrplot(cordat, p.mat = stats$p,method='color',sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")

cordat<-cor(as.matrix(kitsin.std),use="complete.obs")
stats<-cor.mtest(cordat)
corrplot(cordat, p.mat = stats$p,method='color',sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")
corrplot(cordat, p.mat = stats$p,method='number',pch=2,sig.level = c(.001, .01, .05), pch.cex=0.9, insig = "label_sig", type="upper")



kitsin.nona<-na.omit(kitsin.std)
pass1<-princomp(kitsin.nona)
summary(pass1)
loadings(pass1)
biplot(pass1)
#####
#summary(lm(above_ground_dry_yield~+lai+Edge.+z+Score, data=dat.flood))

#dat.noflood<-dat.flood[dat.flood$Edge.=="0",]
# summary(lm(above_ground_dry_yield~lai+z+Score, data=dat.noflood))
# 
# summary(lm(above_ground_dry_yield~z, data=dat.noflood))
# summary(lm(above_ground_dry_yield~z, data=dat.noflood))


# boxplot(above_ground_dry_yield~set_id, data=dat.check)
# biom<-aov(above_ground_dry_yield~set_id, data=dat.check)
# summary(biom)
# summary(lm(above_ground_dry_yield~row_density+height+lai+Edge+z+Score, data=dat.check))
# 
# library(lme4)
# summary(lmer(above_ground_dry_yield~row_density+height+lai+Edge+z+Score+(1|set_id), data=dat.check))
# 
# 
# summary(lmer(above_ground_dry_yield~z+lai+(1|set_id), data=dat.check))
# 
# esquisser()
# #####
# 
# #Bimodal effect of altitude####
# library(ggplot2)
# 
# which(dat.flood$z==max(dat.flood$z[dat.flood$Edge.==1]))
# 
# dat.flood$Z[319]<-NA
# 
# ggplot(dat.flood) +
#  aes(x = z, y = above_ground_dry_yield, colour = Edge.) +
#  geom_point(size = 1.78) +
#  geom_smooth(span = 1L, method='lm') +
#  scale_color_brewer(palette = "Dark2") +
#  labs(x = "Altitude", y = "Yield", color = "Flood") +
#  theme_minimal()
# 
# 
# summary(lm(dat.flood$above_ground_dry_yield[dat.flood$Edge.==1]~dat.flood$z[dat.flood$Edge.==1]))
# 
# ggplot(dat.lp) +
#   aes(x = row, y = range, fill = z) +
#   geom_tile(size = 1L) +
#   scale_fill_distiller(palette = "Spectral") +
#   theme_minimal()
# #####
# 
# 
# 
# #Run normalizeLP before this
# library(ggplot2)
# 
# #1000 x 475 works alright for output
# 
# #Phenome tile plot for albedo####
# ggplot(dat.lp) +
#   aes(x = row, y = range, fill = vis.400.700) +
#   geom_tile(size = 1L) +
#   scale_fill_gradientn(colours=c('gray20', '#6C879A', '#B0C2D0', 'aliceblue', 'white'), na.value='mistyrose') +
#   labs(fill="Reflectance \n(300 - 700nm)", x= "Row", y="Range")+
#   theme_minimal(base_size = 20)
# 
# #B0C2D0 original
# #4D7B9A bluer and darker
# #6C879A darker but not much bluer
# #####
# 
# ##Phenome tile plot for 50% canopy depth####
# 
# #Function that extracts the light values at 50% can. height
# extract.canval<-function(dat.h, dat.l, pct=0.5, res=20){
#   holder<-rep(-1, nrow(dat.l))
#   
#   for(i in 1:nrow(dat.l)){
#     
#     heightstr<-approx(as.numeric(dat.h[i,]), n=res)$y
#     datstr<-approx(as.numeric(dat.l[i,]), n=res)$y
#     
#     ind<-which(abs(heightstr-pct)==min(abs(heightstr-pct)))
#     at.pct<-datstr[ind]
#     
#     holder[i]<-at.pct
#   }
#   
#   return(holder)
# } 
# pcts<-extract.canval(dat.h=height.rel, dat.l=dat.rel, res=100) #Apply it
# 
# plotdat.ph<-cbind(dat.lp, pcts)
# 
# 
# ggplot(plotdat.ph) +
#   aes(x = row, y = range, fill = pcts) +
#   geom_tile(size = 1L) +
#   scale_fill_gradientn(colours=c("#3A4919","#7B883F","Lightgoldenrod1","white"), na.value='mistyrose', breaks=c(0.1,0.5,0.75,1)) +
#   labs(fill="Proportion \nfull sunlight at \n50% canopy depth", x= "Row", y="Range")+
#   theme_minimal(base_size=20)
# #####
# 
# 
# 
# 
# 
# plot(pcts~dat.lp$vis.400.700)
# 
# plot(pcts~dat.lp$height)
# plot(dat.lp$vis.400.700~dat.lp$height)
# 
# smoothScatter(pcts~dat.lp$above_ground_dry_yield)
#####
fact.dat<-kitsin.nona[,c(1:3, 5:10, 12)] #Full (minus yield, dummies). Can add/remove curvefit steepness (11) for slightly different results
fact.dat.r<-kitsin.nona[, c(1:3, 5:6, 9:13)] #uses vis/nir ratio instead of individual values
fact.dat.t<-kitsin.nona[,c(1:3, 5:8, 12)] #uses regular vis/nir but thins light vars to interception efficiency
fact.dat.tr<-kitsin.nona[,c(1:3, 5:6,12:13)] #both; ratio vis/nir and thinned light vars
fact.cor<-cov.wt(fact.dat)
fact.n<-factanal(x=fact.dat, factors=6, n.obs=960); fact.n
#fact.c<-factanal(covmat=fact.cor, factors=3, n.obs=960)

comp.dat<-kitsin.nona[,c(1:10, 12, 14, 16)]
thincomp<-princomp(comp.dat)
summary(thincomp)
loadings(thincomp)
biplot(thincomp)
