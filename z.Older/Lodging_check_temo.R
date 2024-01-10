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


