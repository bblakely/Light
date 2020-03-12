dat.check.bk<-dat.check
dat.check<-dat.check[dat.check$Edge==0,]
dat.lp.bk<-dat.lp
dat.lp<-dat.lp[dat.lp$Edge==0,]
dat.print.bk<-dat.print
dat.print<-dat.print[dat.print$Flood==0,]

# boxplot(dat$height~as.factor(dat$set_id), ylim=c(1,2.2))
# abline(h=quantile(dat.lp$height, c(0.05, 0.95)))
# 
# boxplot(dat$Sum_lbs~as.factor(dat$set_id))
# abline(h=quantile(dat.lp$Sum_lbs, c(0.05, 0.95), na.rm=TRUE))
# 
# boxplot(dat$lai~as.factor(dat$set_id))
# abline(h=quantile(dat.lp$lai, c(0.05, 0.95), na.rm=TRUE))

plotvar<-function(dat.full, dat.check, varname){
  
  dat.check.fac=dat.check$set_id
  dat.full.v<-dat.full[,colnames(dat.full)==varname]
  dat.check.v<-dat.check[,colnames(dat.check)==varname]
  
  rg<-sort(dat.full.v)
  means<-aggregate(dat.check.v,by=list(dat.check.fac), FUN='mean', na.rm=TRUE)$x#boxplot(dat$height~as.factor(dat$set_id), plot=FALSE)$out
  ats<-rep(0, 6)
  for(i in 1:6){
    ats[i]<-which(abs((rg)-means[i])==min(abs(rg-means[i])))[1]
  }

  if(varname=='2'){plot(sort(dat.full.v), ylab=varname, xaxt='n', xlab='', col='white', ylim=c(0,1))}else{
  plot(sort(dat.full.v), ylab=varname, xaxt='n', xlab='', col='white')};
  
  abline(v=quantile(1:nrow(dat.full), seq(from=0, to=1, by=0.1)), lty=4, col='gray'); 
  abline(h=unname(quantile(dat.full.v, seq(from=0, to=1, by=0.1), na.rm=TRUE)), lty=4, col='gray')
  points(sort(dat.full.v))
  
  boxplot(dat.check.v~factor(as.factor(dat.check.fac)),
          at=ats, add = TRUE, boxwex=7, ylab='')
  
  
}


par(mfrow=c(2,2), mar=c(3,4,2,1))
plotvar(dat.full=dat.lp, dat.check=dat.check, varname="height")
plotvar(dat.lp, dat.check, varname="Sum_lbs")
plotvar(dat.lp, dat.check, varname="lai")
plotvar(dat.lp, dat.check, varname="row_density")

plotvar(dat.lp, dat.check, varname="vis.400.700")


#Check out light measurements
##Need to stretch by height since heights are so different among checkline members####

str.lp<-matrix(nrow=nrow(dat.print), ncol=10)
for(i in 1:nrow(dat.print)){
  
 str.lp[i,]<-approx(as.numeric(dat.print[i,8:13]), n=10)$y

}

str.lp<-data.frame(str.lp)
#colnames(str.lp)<-paste('L',colnames(str.lp))

#remove chk-e outlier
dat.str<-cbind(dat.print, str.lp)
dat.str[572,31:41]<-NA
#####

par(mfrow=c(2,2), mar=c(3,4,2,1))

plotvar(dat.str, dat.str[str_detect(dat.print$set_id, "CHK"),], 'X2')
plotvar(dat.str, dat.str[str_detect(dat.print$set_id, "CHK"),], 'X4')
plotvar(dat.str, dat.str[str_detect(dat.print$set_id, "CHK"),], 'X6')
plotvar(dat.str, dat.str[str_detect(dat.print$set_id, "CHK"),], 'X8')


library(ggplot2)

ggplot(dat.check) +
  aes(x = lai, y = height, colour = set_id) +
  geom_point(size = 2.64) +
  scale_color_hue() +
  theme_minimal()+
  xlim(quantile(dat.lp$lai, c(0.01, 0.99)))+
  ylim(min(dat.lp$height), max(dat.lp$height))+
  stat_ellipse()

#min(dat.lp$lai), max(dat.lp$lai)
ggplot(dat.check) +
  aes(x = row_density, y = height, colour = set_id) +
  geom_point(size = 2.64) +
  scale_color_hue() +
  theme_minimal()+
  stat_ellipse()


ggplot(dat.check) +
  aes(x = height, y = Score, colour = set_id) +
  geom_point(size = 2.64) +
  scale_color_hue() +
  theme_minimal()+
  stat_ellipse()

#Try extracting joint percentiles or things
short<-dat.lp[which(dat.lp$height<quantile(dat.lp$height, 0.25)),]
thin<-dat.lp[which(dat.lp$lai<quantile(dat.lp$lai, 0.25)),]


tall<-dat.lp[which(dat.lp$height>quantile(dat.lp$height, 0.75)),]
bushy<-dat.lp[which(dat.lp$lai>quantile(dat.lp$lai, 0.75)),]

which(short$genotype_name%in%thin$genotype_name)

which(tall$genotype_name%in%bushy$genotype_name)

which(tall$genotype_name%in%thin$genotype_name)

which(short$genotype_name%in%bushy$genotype_name)


#Revisit multiple regression
library(lme4);library(nlme);library(r2glmm)
dat.check$block_id<-as.factor(dat.check$block_id);dat.check$set_id<-as.factor(dat.check$set_id)
testmod<-lmer(Sum_lbs~height+lai+z+Score+(1|set_id)+(1|block_id), data=dat.check)
summary(testmod)

dat.str.chk<-dat.str[str_detect(dat.str$set_id, "CHK"),]
dat.str.chk$block_id<-as.factor(dat.str.chk$block_id);dat.str.chk$set_id<-as.factor(dat.str.chk$set_id)
dat.chk.scl<-scale(dat.str.chk[colnames(dat.str.chk)%in%c("Sum_lbs", "height", "lai", "z", "Score","X5","X7", "row_density")])
dat.chk.scl<-data.frame(cbind(dat.chk.scl, dat.str.chk[,colnames(dat.check)%in%c("block_id", "set_id")]))
#Yield models####

testmod2<-lmer(Sum_lbs~height*lai+z+Score+(1|set_id)+(1|block_id), data=dat.chk.scl)
summary(testmod2)
r2beta(testmod2)

testmod3<-lmer(Sum_lbs~height+lai+z+Score+(1|set_id)+(1|block_id), data=dat.chk.scl)
summary(testmod3)
r2beta(testmod3)


testmod4<-lmer(Sum_lbs~height+lai+z+Score+set_id+(1|block_id), data=dat.chk.scl)
summary(testmod4)
r2beta(testmod4)


#####

#With lp
testmod5<-lmer(X5~height*lai+z+(1|set_id)+(1|block_id), data=dat.chk.scl)
summary(testmod5)
r2beta(testmod5)

testmod6<-lmer(X5~height*lai+z+set_id+(1|block_id), data=dat.chk.scl)
summary(testmod6)
r2beta(testmod6)



