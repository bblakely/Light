
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

ai<-1-(combo.lp$PPF1_Avg/(combo.lp$PPF_above_Avg*(1-combo.lp$vis.400.700))) #Consider including NIR

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


#kitsin.og<-cbind(dat.lp, pcts, doesitfit, propsat,coefs, ei, dat.lp$nir.700.1000/dat.lp$vis.400.700)
kitsin<-cbind(dat.lp, pcts, doesitfit, propsat,coefs, ai, dat.lp$nir.700.1000/dat.lp$vis.400.700)

#colnames(kitsin.og)[c(22:26,29:30, 31:38)]<-c("Row_Stem_Density", "Height", "LAI", "Yield", "Lodging_Score", "Elevation", "VIS_Reflectance","NIR_Reflectance", "Flood_Affected", "Light_at_50","Fit_Type","Proportion_Saturated_Sun","Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")
colnames(kitsin)[c(22:26,29:30, 31:39)]<-c("Row_Stem_Density", "Height", "LAI", "Yield", "Lodging_Score", "Elevation", "VIS_Reflectance","NIR_Reflectance", "NDVI","Flood_Affected", "Light_at_50","Fit_Type","Proportion_Saturated_Sun","Curvefit_Steepness", "Interception_efficiency", "NIR_VIS_ratio")


#Give this better names


library(fastDummies)
kitsin.want<-kitsin[,c(22:26,29:30, 31:39)];
kitsin.dum<-dummy_cols(kitsin.want)[(17:20)]
kitsin.nums<-kitsin.want[,c(1:11, 13:16)]
kitsin.full<-cbind(kitsin.want, kitsin.dum)

kitsin.nums[,5]<-as.numeric(kitsin.nums[,5]); kitsin.nums[,10]<-as.numeric(kitsin.nums[,10])
kitsin.std<-cbind(scale(kitsin.nums, center=TRUE), kitsin.dum)





library(jtools, broom)#, ggstance)
sinkmodel<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected+Lodging_Score+Elevation+NDVI+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Fit_Type+Curvefit_Steepness, data=kitsin)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(sinkmodel)
summ(sinkmodel)
plot_summs(sinkmodel, scale=TRUE, colors="forest green")#colors = "#7B883F")

submodel<-(lm(Yield~Height+LAI+Row_Stem_Density+Flood_Affected+Lodging_Score+Elevation+NDVI+Interception_efficiency+Row_Stem_Density, data=kitsin)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
plot_summs(submodel, scale=TRUE, colors="forest green")#colors = "#7B883F")
summary(submodel)
summ(submodel)

#step(kitsin.std)
step(sinkmodel)
# selectmodel<-lm(formula = Yield ~ Height + LAI + Lodging_Score + Elevation + 
#                   VIS_Reflectance + NIR_Reflectance + Interception_efficiency + 
#                   Light_at_50, data = kitsin)


selectmodel<-lm(formula = Yield ~ Row_Stem_Density + Height + LAI + Lodging_Score + Elevation + 
                                   NDVI + Interception_efficiency+Proportion_Saturated_Sun,
                                   data = kitsin)
                  
summary(selectmodel)
summ(selectmodel)
plot_summs(selectmodel, scale=TRUE, colors="forest green")#colors = "#7B883F")

# 
# sinkmodel.d<-(lm(Yield~Height+LAI+Row_Stem_Density+Flood_Affected_1+Lodging_Score+Elevation+NDVI+Interception_efficiency+Light_at_50+Fit_Type_E+Curvefit_Steepness, data=kitsin.std)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
# summary(sinkmodel.d)
# summ(sinkmodel.d)
# plot_summs(sinkmodel.d, scale=TRUE, colors="forest green")#colors = "#7B883F")
# 
# step(sinkmodel.d)
# selectmodel.d<-lm(formula = Yield ~ Height + LAI + Lodging_Score + Elevation + 
#      NIR_VIS_ratio + Interception_efficiency, data = kitsin.std)
# summary(selectmodel.d)
# summ(selectmodel.d)
# plot_summs(selectmodel.d, scale=TRUE, colors="forest green")#colors = "#7B883F")

#Structure explains some 31%, environment


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
