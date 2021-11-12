#Script with parts from 'score' that scales light and height 0-1

source('LightProf_miniplot.R') 

dat.c<-dat.lp[,which(colnames(dat.lp)%in%ppf.names)]



height.dat<-cliplp.dat<-data.frame(matrix(nrow=nrow(dat.c), ncol=6)) #Two df's of same size


for (i in (c(1:nrow(dat.c)))){
  
  plot<-as.numeric(dat.c[i,])
  
  ##QC####
  #Calcualte light absorbed at each level. e.g. S6 - S5 = light absorbed by layer 5
  atten<-diff(plot)
  
  #If S6 is below S5, set both to the highest value. Temporary solution.
  if(atten[5]<0|is.na(atten[5])){plot[5:6]<-max(plot[5:6], na.rm=TRUE)}
  
  atten<-diff(plot) #reset atten after correction
  
  #If any other lower levels are brighter than higher levels, set both to the mean of the two
  err<-which(atten<0) #Finds the lower layer with the too-high value
  if(length(err)!=0){plot[c(err, err+1)]<-mean(c(plot[err], plot[err+1]))} #Set it to the mean 
  #####
  
  ##Clip to actual height of plant####
  if(length(which(heights>(dat.lp$height[i]*100)))>1){ #If there is at least one measurement point above the known height of the plant...
    above<-which(heights>dat.lp$height[i]*100)[2:length(which(heights>dat.lp$height[i]*100))] #Points to discard. Starts at the second sensor above the measured height because the top of canopy is receiving the amount of sun measured by the first sensor above it
    plot.new<-plot;plot.new[above]<-NA #NaN sensor readings known to be above plant
  }else{plot.new<-plot}
  
  cliplp.dat[i,]<-plot.new #Sets new light values
  
  height.new<-c(heights[which(heights<dat.lp$height[i]*100)], dat.lp$height[i]*100) #Height of sensors below top of canopy + height at top of canopy
  length(height.new)<-6 #Will add NA's to vector until it's 6 long
  height.dat[i,]<-height.new #Set new height values; will be same length as light values
  #####

}


#Heights and light levels scaled: 
dat.rel<-data.frame(t(apply(cliplp.dat, 1, function(x) x/max(x, na.rm=TRUE))))
height.rel<-data.frame(t(apply(height.dat, 1, function(x) x/max(x, na.rm=TRUE))))


#Reverse the scales for plotting
dat.scale<-data.frame(t(apply(dat.rel, 1, function(x) rev(x)))) #light starting from the top
height.scale<-data.frame(t(apply(height.rel, 1, function(x) rev(as.numeric(max(x, na.rm=TRUE)-x))))) #height starting from the top

#set up axis for reuse
#plot(c(1:10), ylim=c(0,1), xlim=c(0,1), col='white', xaxt='n')
#axis(side=1, at=seq(0, 1, by=0.2), labels=rev(seq(0, 1, by=0.2)))

doesitfit<-rep('n', 960);propsat<-rep(1, 960);coefs=rep(999, 960)
fake<-data.frame(seq(from=0, to=1, by=0.05));colnames(fake)<-'height.norm' #For smooth plotting

sat.sun<-0.46 #Fraction full sun considered saturating


#Curvefitting

fiteq<-expression(e^~(k~'*'~height))

select<-which(dat.lp$doy==234)#&dat.lp$line==1&dat.lp$range==20)
plotsamp<-select#sample(select)#<-sample(1:960, 27); plotsamp<-c(211,226)

par(mfrow=c(4,4))
for(i in select){ #1:960 for all
  
  if(i%in%plotsamp){
    #plot(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])), xlab='height', ylab='light', main=i)
    plot(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])), xlab='Depth (scaled) ', ylab='Light (scaled)', pch=19,cex=1.5, xlim=c(0, 1), main=paste("line", dat.lp$line[i], "range", dat.lp$range[i]))
    lines(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])), lty=2, col="#3A4919",lwd=2)
    #legend(0.75, 1,legend=c("observations",fiteq), lwd=c(NA,2), pch=c(19,NA), col=c( 'black','#7B883F'), bty='n', seg.len=1)
    }
  
  curve<-as.numeric(dat.scale[i,]);height.norm<-as.numeric(height.scale[i,])
  AICexp<-AIClog<-AIClm<-999
  
  tryCatch({  
    fit3.b<-nls(curve ~ exp(k*height.norm), start=c(k=-1)) #exponential, red
    if(exists('fit3.b')){
      if(i%in%plotsamp){lines(fake$height.norm, predict(fit3.b, newdata=fake), col='#7B883F', lwd=2)}
      AICexp<-round(AIC(fit3.b), 3); #text(0.2, 1000, paste("EXP= ", AICexp), cex=0.8)
      doesitfit[i]<-'E';coefs[i]<-summary(fit3.b)$coefficients[1]; if(i%in%plotsamp){text(0.9, 0.95, paste("k =",round(coefs[i], 2)), cex=1.3)}
      propsat[i]<-approx(x=predict(fit3.b, newdata=fake),y=fake$height.norm,xout=sat.sun)$y
      approx(x, y=x,xout=0.97)
      
      rm('fit3.b')
    }
  }, error=function(e){
    #print("Exp failed to fit")
  })
  
  tryCatch({  
    fit3.c<-nls(curve~SSlogis(height.norm, Asym,xmid,scal))
    if(exists('fit3.c')){
      if(i%in%plotsamp){lines(fake$height.norm, predict(fit3.c, newdata=fake), col='blue')}
      AIClog<-round(AIC(fit3.c), 3); #text(0.2, 1000, paste("EXP= ", AICexp), cex=0.8)
      sat.log<-approx(x=predict(fit3.c, newdata=fake),y=fake$height.norm,xout=sat.sun)$y
      rm('fit3.c')
    }
  }, error=function(e){
    #print("Log failed to fit")
  })
  
  tryCatch({  
    fit3.d<-lm(curve~height.norm)
    if(exists('fit3.d')){
      if(i%in%plotsamp){lines(fake$height.norm, predict(fit3.d, newdata=fake), col='red')}
      AIClm<-round(AIC(fit3.d), 3); #text(0.2, 1000, paste("EXP= ", AICexp), cex=0.8)
      sat.log<-approx(x=predict(fit3.d, newdata=fake),y=fake$height.norm,xout=sat.sun)$y
      rm('fit3.d')
    }
  }, error=function(e){
    #print("Log failed to fit")
  })
  
  fitlist<-c("L", "E", "N")
  doesitfit[i]<-fitlist[which(c(AIClog, AICexp, AIClm)==min(AIClog, AICexp, AIClm))][]
  
  
  # if(exists('AIClog')&exists('AICexp')&exists('AIClm')&exists('sat.log')){
  #   if(AIClog<AICexp){
  #     doesitfit[i]<-'L'
  #     propsat[i]<-sat.log}
  # }
  
  #if(i%in%plotsamp){text(0.05,0.2,doesitfit[i])}
  
  
  
  
  if(i %% 50 == 0){print(i)}
  rm('AICexp','AIClog')
  
}

length(which(doesitfit=='E'))/length(doesitfit)->prope
length(which(doesitfit=='L'))/length(doesitfit)->propl

coefs<-coefs[select]; coefs[coefs<(-10)]<-NA
fit<-doesitfit[select]

dat.lp.coef<-as.data.frame(as.factor(dat.lp$line[select])); dat.lp.coef$k<-coefs; dat.lp.coef$fit<-fit; colnames(dat.lp.coef)[1]<-"line"
par(mfrow=c(1,1))

boxplot(data=dat.lp.coef, k~line)
kext<-aov(data=dat.lp.coef,k~line)
summary(kext)
TukeyHSD(kext)

library(ggplot2)
ggplot(dat.lp.coef) +
  aes(x = line, fill = fit) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

par(mfrow=c(1,2))
plot(density(propsat, na.rm=TRUE), main='% saturated', xlab="Proportion plant height in saturating sun",
     lwd=2, font.axis=2, font.lab=2)
plot(density(coefs[fit=="E"&!is.na(coefs)]), main='coefficients', xlim=c(-30,0), xlab="Value of exponential decay (e^kx) coefficient k",
     lwd=2, font.axis=2, font.lab=2, na.rm=TRUE)


#Make a sheet with all the data to be used elsewhere, e.g. with lodging
colnames(dat.scale)<-c("L6", "L5", "L4", "L3", "L2", "L1")
dat.print<-cbind(dat.lp[c(1:8)], dat.scale, dat.lp[15:32])
colnames(dat.print)[ncol(dat.print)]<-"Flood"


#write.csv(dat.print, "D:/R/TERRA_09.csv", row.names=FALSE)


#Just for plotting experiments:

pct=c(0,0.2,0.4,0.6,0.8,0.9,0.95,1)
extract.canval<-function(dat.h, dat.l, pct=0.5, res=50){
  holder<-data.frame(matrix(data=-1, nrow(dat.l), ncol=length(pct)))
  
  for(i in 1:nrow(dat.l)){
    
    heightstr<-approx(as.numeric(dat.h[i,]), n=res)$y; #plot(heightstr)
    datstr<-approx(as.numeric(dat.l[i,]), n=res)$y;#plot(datstr)
    
    for(n in 1:length(pct)){
      ind<-which(abs(heightstr-pct[n])==min(abs(heightstr-pct[n])))
      #ind<-which(heightstr-(1-pct[n]))
      at.pct<-datstr[ind]
      
      holder[i,n]<-at.pct
      if(i%/%10==0){print(i)}
    }
  }
  
  return(holder)
}

#Extract sun at 50
pcts<-extract.canval(dat.h=height.rel, dat.l=dat.rel, res=100)

dat.lp$pcts<-unlist(unname(pcts))

par(mar=c(4,4,2,1))
for(d in unique(dat.lp$doy)){

select<-which(dat.lp$doy==d)#&dat.lp$line==1&dat.lp$range==20)
par(mfrow=c(3,2))

dat.scale.doy<-dat.scale[select,]; dat.lp.doy<-dat.lp[select,]; height.scale.doy<-height.scale[select,]
linemeans<-heightmeans<-linesd<-data.frame(matrix(nrow=6, ncol=6))
linemeans.alt<-standsd<-data.frame(matrix(nrow=6, ncol=length(pct)))

standard.ht<-extract.canval(dat.h=(1-height.scale.doy), dat.l=dat.scale.doy, pct=pct)
# for(i in 1:nrow(standard.ht)){
#   plot(as.numeric(dat.scale.doy[i,])~as.numeric(height.scale.doy[i,]), type='l')
#   points(rev(as.numeric(standard.ht[i,]))~rev(1-pct))
# }

for(i in sort(unique(dat.lp.doy$line))){ #1:960 for all
  
  linedat<-dat.scale.doy[dat.lp.doy$line==i,]; heightdat<-height.scale.doy[dat.lp.doy$line==i,]
  stdat<-standard.ht[dat.lp.doy$line==i,]; colnames(stdat)<-rev(pct)

  plot(as.numeric(dat.scale.doy[1,])~(as.numeric(height.scale.doy[1,])), xlab='Depth (scaled) ', ylab='Light (scaled)', pch=19,cex=1.5, xlim=c(0, 1), main=paste("line", i, "doy", d), col='white')
  
  for(p in 1:nrow(linedat)){

    points(as.numeric(linedat[p,])~(as.numeric(heightdat[p,])), pch=19, col=i+1)
    #lines(as.numeric(linedat[p,])~(as.numeric(heightdat[p,])),lty=2, col=i+1)
    
    }
  
  #lines(colMeans(linedat)~colMeans(heightdat), lwd=3, col=i+1)
  
  lines(colMeans(rev(stdat))~rev(1-pct), lwd=3, col=i+1)
  linemeans.alt[i+1,]<-as.numeric(colMeans(rev(stdat)))
  standsd[i+1,]<-apply(stdat, 2, sd)
  
  
  linemeans[i+1,]<-colMeans(linedat, na.rm=TRUE); heightmeans[i+1,]<-colMeans(heightdat, na.rm=TRUE)
  linesd[i+1,]<-apply(linedat, 2, sd)
  #arrows(x0=as.numeric(heightmeans[i+1,]), x1=as.numeric(heightmeans[i+1,]), y0=as.numeric(linemeans[i+1,])-as.numeric(linesd[i+1,]),y1=as.numeric(linemeans[i+1,])+as.numeric(linesd[i+1,]), angle=90,length=0.1, code=3, col=i+1)
  
  arrows(x0=1-rev(pct), x1=1-rev(pct), y0=as.numeric(linemeans.alt[i+1,])-as.numeric(standsd[i+1,]),y1=as.numeric(linemeans.alt[i+1,])+as.numeric(standsd[i+1,]), angle=90,length=0.1, code=3, col=i+1)
  
  }

par(mfrow=c(1,1))
plot(as.numeric(linemeans[1,])~as.numeric(heightmeans[1,]), col='white',  ylim=c(0,1), xlim=c(0,1), xlab='Depth (scaled) ', ylab='Light (scaled)')
for(i in c(1:6)){
  #lines(as.numeric(linemeans[i,])~as.numeric(heightmeans[i,]), col=i, lwd=3)
  lines(as.numeric(linemeans.alt[i,])~rev(1-pct),  col=i, lwd=3)
  addon<-sample(seq(from=-0.03, to=0.03, by=0.01),1)
  arrows(x0=(1-rev(pct))+addon, x1=(1-rev(pct))+addon, y0=as.numeric(linemeans.alt[i,])-as.numeric(standsd[i,]),y1=as.numeric(linemeans.alt[i,])+as.numeric(standsd[i,]), angle=90,length=0.1, code=3, col=i)
}
legend(0.7, 1, legend=paste("line",c(0:5)), lwd=2, col=c(1:6), bty='n')

}


#Check phenotypes
for(d in unique(dat.lp$DOY)){
  daydat<-dat.lp[dat.lp$DOY==d,]
  daydat$line<-as.factor(daydat$line)
  bplot<-boxplot(pcts~line, dat=daydat, plot=FALSE)
  
  stat<-aov(pcts~line, data=daydat)
  xd<-TukeyHSD(stat);xd
  letlab<-generate_label_df(xd, 1)
  over <- 0.1*max(bplot$stats[nrow(bplot$stats),], na.rm=TRUE)
  boxplot(formula=pcts~line, dat=daydat, main=paste ("Light at 50 DOY", d),  ylim=c(0,1.2))
  text( c(1:nlevels(daydat$line)) , bplot$stat[nrow(bplot$stats),]+over , letlab[,1])
  
  #print(paste("DOY", d))
  #print(summary(stat))
  
  daydat<-dat.lp[dat.lp$DOY==d,]
  daydat$line<-as.factor(daydat$line)
  bplot<-boxplot(ie~line, dat=daydat, plot=FALSE)
  
  stat<-aov(ie~line, data=daydat)
  xd<-TukeyHSD(stat);xd
  letlab<-generate_label_df(xd, 1)
  over <- 0.1*max(bplot$stats[nrow(bplot$stats),], na.rm=TRUE)
  boxplot(formula=pcts~line, dat=daydat, main=paste ("Interception efficiency DOY", d), ylim=c(0,1.2))
  text( c(1:nlevels(daydat$line)) , bplot$stat[nrow(bplot$stats),]+over , letlab[,1])
  
  
  #print(paste("DOY", d))
  #print(summary(stat))
}
  
#need to get a light at 50
library(MASS); library(forecast); 
dat<-dat.lp$ie
lam<-BoxCox.lambda(dat)
test<-BoxCox(dat, lambda=lam)
plot(density(test))

  
  
  
  
