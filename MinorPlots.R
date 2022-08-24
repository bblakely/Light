#Script for simple plots and stats for light panel ms


#Quick stat test of lodging, flood

#severe vs mild
t.test(dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Score%in%c(3:5))], dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Score%in%c(1,2))])

#severe vs unaffected
t.test(dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Score%in%c(3:5))], dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Score==0)])

#mild vs unaffected
t.test(dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Score%in%c(1:2))], dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Score==0)])

length(which(dat.lp.mod$Score%in%c(3:5)))
length(which(dat.lp.mod$Score%in%c(1:2)))
length(which(dat.lp.mod$Score==0))

lodge.ind<-dat.lp.mod$Score; lodge.ind[lodge.ind%in%c(1,2)]<-"M"; lodge.ind[lodge.ind%in%c(3:5)]<-"S"

boxplot(dat.lp.mod$above_ground_dry_yield~lodge.ind, ylim=c(0.8, 3.5))

#flooding
t.test(dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Edge==1)], dat.lp.mod$above_ground_dry_yield[which(dat.lp.mod$Edge==0)])

boxplot(dat.lp.mod$above_ground_dry_yield~dat.lp.mod$Edge, ylim=c(1, 3.5))


#nir/vis
plot(density(kitsin$NIR_VIS_ratio, na.rm=TRUE))


summary(lm(Height~Lodging_Score,data=kitsin))


#Quick stem check

t.test(kitsin$Row_Stem_Density~kitsin$Flood_Affected, alternative="greater")



#Model predicting lodging#####
library(vioplot)

lodge.sinkmodel<-(lm(Lodging_Score~Height+LAI+Flood_Affected_1+Row_Stem_Density+Elevation+NIR_VIS_ratio+Interception_efficiency+Light_at_50+Proportion_Saturated_Sun+Curvefit_Steepness, data=kitsin.std)) # Light_at_50+Fit_Type+Proportion_Saturated_Sun+Curvefit_Steepness[currently problems with those columns]
summary(lodge.sinkmodel)
step(lodge.sinkmodel)


lodge.step<-lm(formula = Lodging_Score ~ Row_Stem_Density + Height + LAI + 
                 Flood_Affected_1 + Elevation + NIR_VIS_ratio + Proportion_Saturated_Sun, 
               data = kitsin.std)

summary(lodge.step);plot_summs(lodge.step)


lodge.sparse<-lm(formula = Lodging_Score ~ Row_Stem_Density + Height + Flood_Affected_1+ NIR_VIS_ratio, 
                 data = kitsin.std)

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

plot(resp~risk, ylab="chance of moderate to severe lodging", xlab='height at DOY 212')
lines(yp~risk)


vioplot(kitsin$Lodging_Score~test, names=paste(splits[1:8], '-', splits[2:9]),
        xlab='height (binned)', ylab='lodging score')

#y<-0.0005*(4^2^risk)
#lines(y~risk)

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
#####  

#General show of damage
par(mfrow=c(1,3))
vioplot(kitsin$Yield~kitsin$Flood_Affected, ylim=c(1, 4),names=c("no flooding","flooding"), ylab='Yield', xlab="flood status")
vioplot(kitsin$Yield~lodge.ind, ylim=c(1, 4), names=c("none to mild", "moderate to severe"), xlab='lodging status', ylab="Yield")

# #With double-smashed plants out (makes little difference)
# par(mfrow=c(1,2))
# vioplot(kitsin$Yield~kitsin$Flood_Affected, subset=which(lodge.ind==0),ylim=c(1, 4),names=c("no flooding","flooding"), ylab='Yield', xlab="flood status")
# vioplot(kitsin$Yield~lodge.ind, ylim=c(1, 4),subset=which(kitsin$Flood_Affected==0), names=c("none to mild", "moderate to severe"), xlab='lodging status', ylab="Yield")


#Relationship between height and lodging
plot(resp~risk, ylab="chance of moderate to severe lodging", xlab='height at DOY 212')
lines(yp~risk)



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


library(esquisse)
esquisser()


kitsin %>%
  filter(LAI >= 1L & LAI <= 7L) %>%
  ggplot() +
  aes(
    x = Proportion_Saturated_Sun,
    y = Yield,
    colour = LAI,
    size = Height
  ) +
  geom_point(shape = "circle") +
  scale_color_distiller(palette = "RdBu", direction = 1) +
  theme_minimal()


#Trendlines
all<-ggplot(kitsin) +
  aes(x = Proportion_Saturated_Sun, y = Yield) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  theme_minimal() +
  xlim(0.02, 0.97) +
  ylim(0.5, 4)+
  xlab(" ")+
  annotate(geom="text", x=0.75, y=3.75, label="IE>0.44 (all)", color="black",size=5 )+
  geom_smooth(method=lm, col='black')

p99<-kitsin %>%
  filter(Interception_efficiency >= 0.99 & Interception_efficiency <= 1) %>%
  ggplot() +
  aes(x = Proportion_Saturated_Sun, y = Yield) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  theme_minimal() +
  xlim(0.02, 0.97) +
  ylim(0.5, 4)+
  xlab(" ")+
  annotate(geom="text", x=0.75, y=3.75, label="IE>0.99 (37%)", color="black",size=5)+
  geom_smooth(method=lm, col='black')


p995<-kitsin %>%
  filter(Interception_efficiency >= 0.995 & Interception_efficiency <= 1) %>%
  ggplot() +
  aes(x = Proportion_Saturated_Sun, y = Yield) +
  geom_point(shape = "circle", size = 1.5, colour = "black") +
  theme_minimal() +
  xlim(0.02, 0.97) +
  ylim(0, 4)+
  annotate(geom="text", x=0.75, y=3.75, label="IE>0.995 (8%)", color="black",size=5 )+
  geom_smooth(method=lm, col='black')


#library(gridExtra)
grid.arrange(all, p99, p995)

