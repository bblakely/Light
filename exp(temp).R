

combo.lp<-cbind(dat.lp, dat.rel)
esquisser()

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