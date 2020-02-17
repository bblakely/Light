

combo.lp<-cbind(dat.lp, dat.rel)

#Height and yield####

library(ggplot2)

ggplot(dat.lp) +
  aes(x = row, y = range, fill = Sum_lbs) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "RdGy") +
  theme_minimal()



ggplot(dat.lp) +
  aes(x = lai, y = Sum_lbs, colour = height) +
  geom_point(size = 1.58) +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

#####

##Linear modeling####
par(mfrow=c(1,1))
plot(dat.lp$height~dat.lp$Sum_lbs)

#kitchen sink dat b
#need random effect on genotype?
#What else might explain variability?

summary(lm(Sum_lbs~row_density+height+lai+Edge+z+Score, data=dat.lp))


summary(lm(Sum_lbs~+lai+Edge.+z+Score, data=dat.flood))

dat.noflood<-dat.flood[dat.flood$Edge.=="0",]
summary(lm(Sum_lbs~lai+z+Score, data=dat.noflood))

summary(lm(Sum_lbs~z, data=dat.noflood))
summary(lm(Sum_lbs~z, data=dat.noflood))


boxplot(Sum_lbs~set_id, data=dat.check)
biom<-aov(Sum_lbs~set_id, data=dat.check)
summary(biom)
summary(lm(Sum_lbs~row_density+height+lai+Edge.+z+Score, data=dat.check))

library(lme4)
summary(lmer(Sum_lbs~row_density+height+lai+Edge.+z+Score+(1|set_id), data=dat.check))


summary(lmer(Sum_lbs~z+lai+(1|set_id), data=dat.check))

esquisser()
#####

#Bimodal effect of altitude####
library(ggplot2)

which(dat.flood$z==max(dat.flood$z[dat.flood$Edge.==1]))

dat.flood$Z[319]<-NA

ggplot(dat.flood) +
 aes(x = z, y = Sum_lbs, colour = Edge.) +
 geom_point(size = 1.78) +
 geom_smooth(span = 1L, method='lm') +
 scale_color_brewer(palette = "Dark2") +
 labs(x = "Altitude", y = "Yield", color = "Flood") +
 theme_minimal()


summary(lm(dat.flood$Sum_lbs[dat.flood$Edge.==1]~dat.flood$z[dat.flood$Edge.==1]))

ggplot(dat.lp) +
  aes(x = row, y = range, fill = z) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()
#####



#Run normalizeLP before this
library(ggplot2)

#1000 x 475 works alright for output

#Phenome tile plot for albedo####
ggplot(dat.lp) +
  aes(x = row, y = range, fill = vis.400.700) +
  geom_tile(size = 1L) +
  scale_fill_gradientn(colours=c('gray20', '#6C879A', '#B0C2D0', 'aliceblue', 'white'), na.value='mistyrose') +
  labs(fill="Reflectance \n(300 - 700nm)", x= "Row", y="Range")+
  theme_minimal(base_size = 20)

#B0C2D0 original
#4D7B9A bluer and darker
#6C879A darker but not much bluer
#####

##Phenome tile plot for 50% canopy depth####

#Function that extracts the light values at 50% can. height
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
pcts<-extract.canval(dat.h=height.rel, dat.l=dat.rel, res=100) #Apply it

plotdat.ph<-cbind(dat.lp, pcts)


ggplot(plotdat.ph) +
  aes(x = row, y = range, fill = pcts) +
  geom_tile(size = 1L) +
  scale_fill_gradientn(colours=c("#3A4919","#7B883F","Lightgoldenrod1","white"), na.value='mistyrose', breaks=c(0.1,0.5,0.75,1)) +
  labs(fill="Proportion \nfull sunlight at \n50% canopy depth", x= "Row", y="Range")+
  theme_minimal(base_size=20)
#####



