#Script for simple plots and stats for light panel ms


#Quick stat test of lodging, flood

Score2num<-kitsin$Lodging_Score2;
Score2num[which(is.na(Score2num))]<-0
Score2num[Score2num>0&Score2num<=40]<-1; Score2num[Score2num>40&Score2num<=70]<-2
Score2num[Score2num>70]<-3;

kitsin$Score2num<-Score2num

#severe vs mild
t.test(kitsin$Yield[which(kitsin$Score2num%in%c(3))], kitsin$Yield[which(kitsin$Score2num%in%c(1:2))])
  
#affected vs unaffected
t.test(kitsin$Yield[which(kitsin$Score2num%in%c(1:3))], kitsin$Yield[which(kitsin$Score2num==0)])

#mild vs unaffected
t.test(kitsin$Yield[which(kitsin$Score2num%in%c(1:2))], kitsin$Yield[which(kitsin$Score2num==0)])



length(which(kitsin$Score2num%in%c(3)))
length(which(kitsin$Score2num%in%c(1:2)))
length(which(kitsin$Score2num==0))

lodge.ind<-kitsin$Score2num; lodge.ind[lodge.ind%in%c(1,2)]<-"M"; lodge.ind[lodge.ind%in%c(3)]<-"S"

boxplot(kitsin$Yield~lodge.ind, ylim=c(0.8, 3.5))

#flooding
t.test(kitsin$Yield[which(dat.lp.mod$Edge==1)], kitsin$Yield[which(dat.lp.mod$Edge==0)])

boxplot(kitsin$Yield~dat.lp.mod$Edge, ylim=c(1, 3.5))


#nir/vis
plot(density(kitsin$NIR_VIS_ratio, na.rm=TRUE))


summary(lm(Height~Lodging_Score,data=kitsin))


#Quick stem check

t.test(kitsin$Row_Stem_Density~kitsin$Flood_Affected, alternative="greater")



#Model predicting lodging#####
library(vioplot)

lodge.sinkmodel<-(lm(Lodging_Score~Height+LAI+Flood_Affected_1+Row_Stem_Density+Elevation+NDVI+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.std.norm)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(lodge.sinkmodel)
lodge.step<-step(lodge.sinkmodel)
plot_summs(lodge.sinkmodel)


#lodge.step<-lm(formula = Lodging_Score2 ~ Height + LAI + 
                # Flood_Affected_1 + Elevation + NDVI + Proportion_Saturated_Sun, 
               #data = kitsin.std)

summary(lodge.step);plot_summs(lodge.step)


lodge.sparse<-lm(formula = Lodging_Score ~ Row_Stem_Density + Height + Flood_Affected_1+ NIR_VIS_ratio, 
                 data = kitsin.std.norm)

summary(lodge.sparse);plot_summs(lodge.sparse)
#plot(Height~Lodging_Score, dat=kitsin); abline(coefficients(lm(Height~Lodging_Score, dat=kitsin)))
#plot(Lodging_Score~Height, dat=kitsin); abline(coefficients(lm(Lodging_Score~Height, dat=kitsin)))

lodge.ind<-rep(0, nrow(kitsin));lodge.ind[which(kitsin$Lodging_Score>=3)]<-1
boxplot(kitsin$Height~lodge.ind)

lodge.agg<-aggregate(Height~Lodging_Score, data=kitsin, FUN='mean')
plot(lodge.agg, ylim=c(quantile(kitsin$Height, c(0.25,0.75)))); abline(h=mean(kitsin$Height))

vioplot(Height~Lodging_Score, data=kitsin)

#let's play...

outcome<-lodge.ind #index of plots with moderate to severe lodging

#Stratify heights

splits<-c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5)
splits2<-c(0.5,  1,  1.5,  2,  2.5)
splits3<-seq(from=0, to=2.5, by=0.2)

test<-as.numeric(cut(kitsin$Height, splits))

resp<-agg<-aggregate(outcome, by=list(test), FUN='mean')$x #odds of moderate to severe lodging at each height
risk<-aggregate(kitsin$Height, by=list(test), FUN='mean')$x #mean heights in each bin


fit<-nls(resp ~ a*exp(k*risk), start=c(a=0.0001, k=5))
#fit<-nls(rest ~ a*(x^z^risk), start=c(a=0.0001, x=4, z=2))
coef<-coefficients(fit);coef
yp<-coef[1]*exp(coef[2]*risk)

plot(resp~risk, ylab="probability of moderate to severe lodging", xlab='height at DOY 212')
lines(yp~risk)


vioplot(kitsin$Lodging_Score~test, names=paste(splits[1:8], '-', splits[2:9]),
        xlab='height (binned)', ylab='lodging score')

#y<-0.0005*(4^2^risk)
#lines(y~risk)

#####

#####

#Models exploring flood recovery#####

#Show how yield affected by flooding

kitsin.flood<-na.omit(kitsin[kitsin$Flood_Affected==1,])
kitsin.flood<-na.omit(kitsin.std[kitsin.std$Flood_Affected_1==1,])

floodsink<-(lm(Yield~Row_Stem_Density+Height+LAI+Lodging_Score+Elevation+NIR_VIS_ratio+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun, data=kitsin.flood)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]

step(floodsink)

flood.select<-lm(Yield ~ Row_Stem_Density + Height + LAI + Lodging_Score + 
                   Light_at_50, data=kitsin.flood)

plot_summs(flood.select)



# #General stats about flooded plants
# t.test(dat.lp.mod$row_density[which(dat.lp.mod$Edge==1)], dat.lp.mod$row_density[which(dat.lp.mod$Edge==0)])
# t.test(dat.lp.mod$height[which(dat.lp.mod$Edge==1)], dat.lp.mod$height[which(dat.lp.mod$Edge==0)])
# t.test(dat.lp.mod$lai[which(dat.lp.mod$Edge==1)], dat.lp.mod$lai[which(dat.lp.mod$Edge==0)])
# t.test(dat.lp.mod$ei[which(dat.lp.mod$Edge==1)], dat.lp.mod$ei[which(dat.lp.mod$Edge==0)])
# t.test(dat.lp.mod$pcts[which(dat.lp.mod$Edge==1)], dat.lp.mod$pcts[which(dat.lp.mod$Edge==0)])
# t.test(dat.lp.mod$propsat[which(dat.lp.mod$Edge==1)], dat.lp.mod$propsat[which(dat.lp.mod$Edge==0)])
# 

#####  

#General show of damage
par(mfrow=c(1,3), mar=c(4.2,4.2,4.2,1))
vioplot(kitsin$Yield~kitsin$Flood_Affected, ylim=c(1, 4),names=c("no flooding","flooding"), ylab='', xlab="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5 ) #flood status
axis(side=1,at=1.5,labels="flood status", line=2, tick=FALSE, cex.axis=1.8); axis(side=2,at=2.5,labels="Yield", line=2, tick=FALSE, cex.axis=1.8)
vioplot(kitsin$Yield~lodge.ind, ylim=c(1, 4), names=c("none to mild", "mod. to severe"), xlab='', ylab="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
axis(side=1,at=1.5,labels="lodging status", line=2, tick=FALSE, cex.axis=1.8); axis(side=2,at=2.5,labels="Yield", line=2, tick=FALSE, cex.axis=1.8)


# #With double-smashed plants out (makes little difference)
# par(mfrow=c(1,2))
# vioplot(kitsin$Yield~kitsin$Flood_Affected, subset=which(lodge.ind==0),ylim=c(1, 4),names=c("no flooding","flooding"), ylab='Yield', xlab="flood status")
# vioplot(kitsin$Yield~lodge.ind, ylim=c(1, 4),subset=which(kitsin$Flood_Affected==0), names=c("none to mild", "moderate to severe"), xlab='lodging status', ylab="Yield")


#Relationship between height and lodging
plot(resp~risk, ylab="prob. of mod. to severe lodging", xlab='height at DOY 212', cex=1.2, cex.axis=1.5, cex.lab=1.8, cex.main=1.5)
lines(yp~risk, lwd=2)

dev.copy(png, width=900, height=375, "plots/Figure6.png")#not printing sized correctly for whatever reason
dev.off()


#Show relationship between light and yield changing

par(mar=c(4,4,1,1), mfrow=c(3,1))
plot(Yield~Proportion_Saturated_Sun, dat=kitsin, ylim=c(1,3.5),xlim=c(0.05,1))
abline(lm(Yield~Proportion_Saturated_Sun, dat=kitsin), col='red'); print(lm(Yield~Proportion_Saturated_Sun, dat=kitsin), col='red')
text(0.8, 3, "IE>0.44 (all)")

plot(Yield~Proportion_Saturated_Sun, dat=kitsin, subset=which(kitsin$Interception_efficiency>0.99), ylim=c(1,3.5), xlim=c(0.05,1))
abline(lm(Yield~Proportion_Saturated_Sun, dat=kitsin,subset=which(kitsin$Interception_efficiency>0.99)), col='red')
text(0.8, 3, "IE>0.99 (37%)")

plot(Yield~Proportion_Saturated_Sun, dat=kitsin, subset=which(kitsin$Interception_efficiency>0.995), ylim=c(1,3.5), xlim=c(0.05,1))
abline(lm(Yield~Proportion_Saturated_Sun, dat=kitsin,subset=which(kitsin$Interception_efficiency>0.995)), col='red')
text(0.8, 3, "IE>0.995 (8%)")

#With Height or LAI as predictor (change subsetting as needed)
#Does not work.

par(mar=c(4,4,1,1), mfrow=c(3,1))
plot(Yield~Proportion_Saturated_Sun, dat=kitsin, ylim=c(1,3.5),xlim=c(0.05,1))
abline(lm(Yield~Proportion_Saturated_Sun, dat=kitsin), col='red'); print(lm(Yield~Proportion_Saturated_Sun, dat=kitsin), col='red')
#text(0.8, 3, "IE>0.44 (all)")

plot(Yield~Proportion_Saturated_Sun, dat=kitsin, subset=which(kitsin$Height>1.5), ylim=c(1,3.5), xlim=c(0.05,1))
abline(lm(Yield~Proportion_Saturated_Sun, dat=kitsin,subset=which(kitsin$Height>1.5)), col='red')
#text(0.8, 3, "IE>0.99 (37%)")

plot(Yield~Proportion_Saturated_Sun, dat=kitsin, subset=which(kitsin$Height>2), ylim=c(1,3.5), xlim=c(0.05,1))
abline(lm(Yield~Proportion_Saturated_Sun, dat=kitsin,subset=which(kitsin$Height>2)), col='red')
#text(0.8, 3, "IE>0.995 (8%)")


library(esquisse)
#esquisser()


#Yield and light negatively related
kitsin.og %>%
  filter(LAI >= 1L & LAI <= 7L) %>%
  ggplot() +
  aes(
    x = Proportion_Saturated_Sun,
    y = Yield,
    colour = LAI,
    size = Height
  ) +
  geom_point(shape = "circle") +
  xlab ("Proportion of canopy in >50% full sunlight")+
  scale_color_distiller(palette = "RdBu", direction = 1) +
  annotate(geom="label", x=0.85, y=0.25, label="more light pentration", color="black",size=8 )+
  annotate(geom="label", x=0.15, y=0.25, label="less light pentration", color="black",size=8 )+
  theme_minimal()+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 24), legend.text = element_text(size = 20),legend.title = element_text(size = 24) )

dev.copy(png,'plots/Fig5a.png', width=900, height=550)
dev.off()

#growth rate and light negatively related
kitsin.og %>%
  filter(LAI >= 1L & LAI <= 7L & Yield>0.5) %>%
  ggplot() +
  aes(
    x = Proportion_Saturated_Sun,
    y = DSM_slp
  ) +
  geom_point(shape = "circle") +
  xlab ("Proportion of canopy in >50% full sunlight")+
  ylab ("Rate of height increase")+
  scale_color_distiller(palette = "RdBu", direction = 1) +
  #annotate(geom="label", x=0.85, y=0, label="more light pentration", color="black",size=3 )+
  #annotate(geom="label", x=0.15, y=0, label="less light pentration", color="black",size=3 )+
  annotate(geom="text", x=0.6, y=0.12, label="Growth rate replacing yield as response variable", color="black",size=4 )+
  theme_minimal()


#Yield and light negatively related even with subset out
kitsin.og %>%
  filter(LAI >= 1L & LAI <= 7L & Lodging_Score<=1 & Flood_Affected==0 & Yield>0.5) %>%
  ggplot() +
  aes(
    x = Proportion_Saturated_Sun,
    y = Yield,
  ) +
  geom_point(shape = "circle") +
  xlab ("Proportion of canopy in >50% full sunlight")+
  scale_color_distiller(palette = "RdBu", direction = 1) +
  #annotate(geom="label", x=0.85, y=0, label="more light pentration", color="black",size=3 )+
  #annotate(geom="label", x=0.15, y=0, label="less light pentration", color="black",size=3 )+
  annotate(geom="text", x=0.65, y=4, label="Flooded and lodged plots excluded", color="black",size=4 )+
  theme_minimal()


dev.copy(png,'plots/Yield_v_Light.png', width=700, height=400)
dev.off()

sunyield<-(lm(Yield~Proportion_Saturated_Sun, data=kitsin.std))
summary(sunyield); coefficients(sunyield)

#Trendlines
all<-ggplot(kitsin.og) +
  aes(x = Proportion_Saturated_Sun, y = Yield) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  theme_minimal() +
  xlim(0.02,1) +
  ylim(0.5, 4)+
  xlab(" ")+
  annotate(geom="text", x=0.75, y=3.75, label="IE>0.44 (all)", color="black",size=5 )+
  geom_smooth(method=lm, col='black')+
  theme(axis.text = element_text(size = 16),axis.title = element_text(size = 14))


p99<-kitsin.og %>%
  filter(Interception_efficiency >= 0.99 & Interception_efficiency <= 1) %>%
  ggplot() +
  aes(x = Proportion_Saturated_Sun, y = Yield) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  theme_minimal() +
  xlim(0.02, 1) +
  ylim(0.5, 4)+
  xlab(" ")+
  annotate(geom="text", x=0.75, y=3.75, label="IE>0.99 (37%)", color="black",size=5)+
  geom_smooth(method=lm, col='black')+
  theme(axis.text = element_text(size = 16),axis.title = element_text(size = 14), legend.text = element_text(size = 16),legend.title = element_text(size = 20) )



p995<-kitsin.og %>%
  filter(Interception_efficiency >= 0.995 & Interception_efficiency <= 1) %>%
  ggplot() +
  aes(x = Proportion_Saturated_Sun, y = Yield) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  theme_minimal() +
  xlim(0.02, 1) +
  ylim(0, 4)+
  xlab("Proportion of canopy in >50% full sunlight")+
  annotate(geom="label", x=0.80, y=0.5, label="more light pentration", color="black",size=4 )+
  annotate(geom="label", x=0.20, y=0.5, label="less light pentration", color="black",size=4 )+
  annotate(geom="text", x=0.75, y=3.75, label="IE>0.995 (8%)", color="black",size=5 )+
  geom_smooth(method=lm, col='black')+
  theme(axis.text = element_text(size = 16),axis.title = element_text(size = 14))



library(gridExtra)
grid.arrange(all, p99, p995)


dev.copy(png,'plots/Fig5_bcd.png', width=400, height=625)#was 350,500
dev.off()

#Bimodal effct of altitude####
library(ggplot2)

ggplot(kitsin) +
  aes(x = row, y = range, fill = Elevation) +
  geom_tile(size = 1.5) +
  scale_fill_distiller(palette = "Spectral", 
                       direction = -1) +
  theme_minimal()

kitsin.elev<-kitsin
kitsin.elev$Flood_Affected[kitsin.elev$Flood_Affected==1]<-"Y"; kitsin.elev$Flood_Affected[kitsin.elev$Flood_Affected==0]<-"N"
kitsin.elev$Elevation<-kitsin.elev$Elevation*0.3048

#kitsin.elev$Flood_Affected[which(kitsin$Elevation>quantile(kitsin$Elevation, 0.50, na.rm=TRUE))]<-2

ggplot(kitsin.elev) +
 aes(x = Elevation, y = Yield, colour = Flood_Affected) +
 geom_point(size = 1.78) +
 geom_smooth(span = 1L, method='lm') +
 scale_color_manual(values=c("black", "dark red")) +
 labs(x = "Elevation (m)", y = "Yield (kg m-2)", color = "Flood?") +
 theme_minimal()+
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        plot.title=element_text(size=26), axis.title=element_text(size=18,face="bold"), legend.title = element_text(size=20))+
 ylim(c(0.5, 4))

dev.copy(png,'C:/Users/Bethany/Desktop/floodelev.png', width=700, height=500)
dev.off()


summary(lm(Yield~Elevation, data=subset(kitsin, Flood_Affected=="1")))
summary(lm(Yield~Elevation, data=subset(kitsin, Flood_Affected=="0")))



#Trait distributions

par(mfrow=c(2,2))
for(i in 1:ncol(kitsin.nums)){
  plot(density(kitsin.nums[,i], na.rm=TRUE), main=colnames(kitsin.nums)[i])
}


ht<-ggplot(kitsin.og) +
  aes(x = Height) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Height (m)", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

lai<-ggplot(kitsin.og) +
  aes(x = LAI) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "LAI", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

ndvi<-ggplot(kitsin.og) +
  aes(x = NDVI) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "NDVI", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

ie<-ggplot(kitsin.og) +
  aes(x = Interception_efficiency) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Interception efficiency", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

pss<-ggplot(kitsin.og) +
  aes(x = Proportion_Saturated_Sun) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Prop. canopy in >50% full sun", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

l50<-ggplot(kitsin.og) +
  aes(x = Light_at_50) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Light at 50% canopy depth", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )


cv<-ggplot(kitsin.og) +
  aes(x = Curvefit_Steepness) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Curvefit steepness param. k", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

ls<-ggplot(kitsin.og)+
  aes(x = Lodging_Score) +
  geom_bar(color = "black",  lwd=1) +
  labs(x = "Lodging score", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

kitsin.pltfld<-kitsin; 
kitsin.pltfld$cat<-"N"
kitsin.pltfld$cat[kitsin$Flood_Affected==1]<-"Y"

fld<-ggplot(kitsin.pltfld)+
  aes(x = cat) +
  geom_bar(color = "black",  lwd=1) +
  labs(x = "Flooded?", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

yl<-ggplot(kitsin.og)+
  aes(x = Yield) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Yield (kg m-2)", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

refl<-ggplot(kitsin.og)+
  aes(x = vis.400.700) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Albedo", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

density<-ggplot(kitsin.og)+
  aes(x = Row_Stem_Density) +
  geom_histogram(bins=30, color = "black", lwd=1) +
  labs(x = "Density", y = "") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L), axis.text = element_text(size=12L)
  )

#library(gridExtra)
grid.arrange(ht, lai, ndvi, refl,ie,cv, pss,l50,  yl, ls,fld)#,pss,ls)

dev.copy(png, "plots/Figure4.png", width=750, height=550)
dev.off()

grid.arrange(ht, lai, ndvi)

