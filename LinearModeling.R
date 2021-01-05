

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

##Linear modeling####
par(mfrow=c(1,1))

plot(dat.lp$height~dat.lp$above_ground_dry_yield)

#kitchen sink dat b
#need random effect on genotype?
#What else might explain variability?

summary(lm(above_ground_dry_yield~row_density+height+lai+Edge+z+Score, data=dat.lp))

#Create 'pcts', originally from phenomeplots
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

#calculate interception efficiency
ei<-1-(combo.lp$PPF1_Avg/combo.lp$PPF_above_Avg)
ei[combo.lp$PPF_above_Avg<combo.lp$PPF5_Avg]<-(1-(combo.lp$PPF1_Avg/combo.lp$PPF5_Avg))[combo.lp$PPF_above_Avg<combo.lp$PPF5_Avg]


kitsin<-cbind(dat.lp, pcts, doesitfit, propsat,coefs, ei)
colnames(kitsin)[c(22:26,29:30, 31:37)]<-c("Row_Stem_Density", "Height", "LAI", "Yield", "Lodging_Score", "Elevation", "VIS_Reflectance","NIR_Reflectance", "Flood_Affected", "Light_at_50","Fit_Type","Proportion_Saturated_Sun","Curvefit_Steepness", "Interception_efficiency" )
#Give this better names
library(jtools, broom)
sinkmodel<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected+Lodging_Score+Elevation+VIS_Reflectance+NIR_Reflectance+Interception_efficiency+Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin)) # [currently problems with those columns]
summary(sinkmodel)
summ(sinkmodel)
plot_summs(sinkmodel, scale=TRUE, colors = "#7B883F")

library(fastDummies)
kitsin.want<-kitsin[,c(22:26,29:30, 31:37)]; #:33, 35:
kitsin.dum<-dummy_cols(kitsin.want)[14:17]
kitsin.nums<-kitsin.want[,c(1:8, 10, 12:14)]
kitsin.std<-cbind(scale(kitsin.nums, center=TRUE), kitsin.dum)

sinkmodel2<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected_1+Flood_Affected_0+Lodging_Score+Elevation+VIS_Reflectance+NIR_Reflectance+Light_at_50+Fit_Type_E+Fit_Type_L+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.std)) #+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness [currently problems with those columns]
summary(sinkmodel2)
summ(sinkmodel2)


submodel<-function(lai.min, lai.max, height.min, height.max, variable) {
#kitsin.subset<-kitsin.want[which(kitsin.want$LAI<lai.max & kitsin.want$LAI>lai.min & kitsin.want$Height>height.min & kitsin.want$Height<height.max), ]
#kitsin.subset.num<-(kitsin.subset[,c(1:8, 10, 12:13)]);kitsin.subset.dum<-dummy_cols(kitsin.subset)[12:16]
#kitsin.subset.std<-cbind(scale(kitsin.subset.num, center=TRUE), kitsin.subset.dum)

kitsin.subset.std<-kitsin.std[which(kitsin.want$LAI<lai.max & kitsin.want$LAI>lai.min & kitsin.want$Height>height.min & kitsin.want$Height<height.max),]
  
submodel2<-(lm(Yield~Row_Stem_Density+Height+LAI+Flood_Affected_0+Lodging_Score+Elevation+VIS_Reflectance+NIR_Reflectance+Light_at_50+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.subset.std)) #+Fit_Type_E+Fit_Type_L+

ind<-which(names(submodel2$coefficients)==variable)
print(lai.min); print(nrow(kitsin.subset.std));print(submodel2$coefficients[ind]); 
pch<-1
if(!is.na(summary(submodel2)$coef[,4][ind])){if(summary(submodel2)$coef[,4][ind]<0.05){pch<-19}}else(pch=3)

points(submodel2$coefficients[ind]~lai.min, pch=pch)
summary(submodel2)

}

plot(1:7, ylim=c(-1, 1), xlim=c(0, 10), col='white')
for (i in c(4:16)){submodel(lai.min=(i*0.5),lai.max = (i+1)*0.5, height.min=1.5,height.max=2.5, variable="NIR_Reflectance")}
colnames(kitsin.want)

summ(submodel2)



library(corrplot)
corrplot(cor(as.matrix(kitsin.nums), use="complete.obs"), method='color')

kitsin.nona<-na.omit(kitsin.std)
pass1<-princomp(kitsin.nona)
summary(pass1)
loadings(pass1)
biplot(pass1)

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
