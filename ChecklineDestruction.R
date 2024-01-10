#Analysis of effect of destructive weather on checklines

dat.check<-kitsin.og[str_detect(kitsin.og$set_id, "CHK"),]

#esquisser()

# ggplot(dat.check) +
#   aes(x = "", y = Yield, fill = set_id) +
#   geom_boxplot(shape = "circle") +
#   scale_fill_hue(direction = 1) +
#   theme_minimal() +
#   facet_wrap(vars(Lodging_Score))+
#   xlab("<- unflooded     flooded ->")


means<-aggregate(dat.check$Yield, by=list(dat.check$set_id), FUN='mean')

#Change flood labelong
dat.chk.plot<-dat.check; 
dat.chk.plot$Flood_Affected[dat.chk.plot$Flood_Affected==0]<-"N"; dat.chk.plot$Flood_Affected[dat.chk.plot$Flood_Affected==1]<-"Y"


ggplot(dat.chk.plot) +
  aes(x = Lodging_Score, y = Yield, colour = Flood_Affected) +
  geom_point(shape = "circle", size = 4) +
  scale_color_grey() +
  theme_bw() +
  stat_smooth(method="lm", colour="black", alpha=0.2)+
  facet_wrap(vars(set_id))+
  xlab("Lodging Score")+
  labs(colour="Flood Affected?")+
  guides(colour = guide_legend(override.aes = list(linetype = 0)))+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 24), legend.text = element_text(size = 20),legend.title = element_text(size = 24), strip.text = element_text(size=20))


dat.stat<-dat.check
dat.stat$Lodging_Score<-as.factor(dat.stat$Lodging_Score)
dat.stat$Flood_Affected<-as.factor(dat.stat$Flood_Affected)

#Seems like a job for anova
cdmod <- aov(Yield ~ set_id * Flood_Affected * Lodging_Score, data = dat.check)
summary(cdmod)


#plot(cdmod)


thing<-aggregate(dat.check$Yield, by=list(dat.check$set_id,dat.check$Flood_Affected), FUN=mean)
y<-thing[thing$Group.2==1,]; n<-thing[thing$Group.2==0,]
mean((y$x-n$x)/n$x)

thing2<-aggregate(dat.check$Yield, by=list(dat.check$set_id,dat.check$Lodging_Score), FUN=mean)

n<-thing2[thing2$Group.2<3,]; y<-thing2[thing2$Group.2>=3,]

ya<-aggregate(y, by=list(y$Group.1), FUN=mean); na<-aggregate(n, by=list(n$Group.1), FUN=mean)[ya$Group.1%in%c("CHK-B", "CHK-D", "CHK-F"),]

mean((ya$x-na$x)/na$x)

#Light and checklines

lcmod <- lm(Yield ~ set_id * Light_at_50, data = dat.trans)


Light_at_50<-boxcox(dat.check$Light_at_50)$x.t

dat.trans<-dat.check; dat.trans$Light_at_50<-Light_at_50

dat.check %>%
  filter(LAI >= 1 & LAI <= 7) %>%
  ggplot() +
  aes(x = Light_at_50, y = Yield, colour = LAI, size = Height) +
  geom_point(shape = "circle") +
  scale_color_distiller(palette = "RdBu", direction = 1) +
  #geom_smooth(data=dat.check[dat.check$set_id%in%c("CHK-B", "CHK-E"),],
              #aes(x = Light_at_50, y = Yield), method="lm", color="black", show.legend = FALSE)+
  theme_minimal() +
  facet_wrap(vars(set_id))+
  theme_bw()+
  theme(axis.text = element_text(size = 18),axis.title = element_text(size = 24), legend.text = element_text(size = 20),legend.title = element_text(size = 24),strip.text = element_text(size=20) )

#Significance?

summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-A")))
thing<-summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-B")))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-C")))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-D")))
thing<-summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-E")))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-F")))

#without destruction
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-A" & Flood_Affected==0 & Lodging_Score<2)))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-B"& Flood_Affected==0& Lodging_Score<2)))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-C"& Flood_Affected==0& Lodging_Score<2)))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-D"& Flood_Affected==0& Lodging_Score<2)))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-E"& Flood_Affected==0& Lodging_Score<2)))
summary(lm(Yield~Light_at_50, data=subset(dat.trans, set_id=="CHK-F"& Flood_Affected==0& Lodging_Score<2)))


