#Script with parts from 'score' that scales light and height 0-1

source('ChecklineVar.R') 

dat.c<-dat.check[colnames(dat.check)%in%ppf.names]
heights<-c(13,50,100,150,200,250)

height.dat<-cliplp.dat<-data.frame(matrix(nrow=nrow(dat.c), ncol=6)) #Two df's of same size


for (i in (c(1:nrow(dat.c)))){
  
  plot<-as.numeric(dat.c[i,]); #plot(plot)
  
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
  if(length(which(heights>(dat.check$height[i]*100)))>1){ #If there is at least one measurement point above the known height of the plant...
    above<-which(heights>dat.check$height[i]*100)[2:length(which(heights>dat.check$height[i]*100))] #Points to discard. Starts at the second sensor above the measured height because the top of canopy is receiving the amount of sun measured by the first sensor above it
    plot.new<-plot;plot.new[above]<-NA #NaN sensor readings known to be above plant
    plot(plot.new)
  }else{plot.new<-plot}
  
  cliplp.dat[i,]<-plot.new #Sets new light values
  
  height.new<-c(heights[which(heights<dat.check$height[i]*100)], dat.check$height[i]*100) #Height of sensors below top of canopy + height at top of canopy
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

doesitfit<-rep('n', nrow(dat.c));propsat<-rep(1, nrow(dat.c));coefs=rep(999, nrow(dat.c))
fake<-data.frame(seq(from=0, to=1, by=0.05));colnames(fake)<-'height.norm' #For smooth plotting

sat.sun<-0.46 #Fraction full sun considered saturating



#Curvefitting

fiteq<-expression(e^~(k~'*'~height))

#select<-which(dat.lp$doy==259)#&dat.lp$line==1&dat.lp$range==20)
plotsamp<-sample(1:nrow(dat.c), 27)

par(mfrow=c(4,4))
for(i in 1:nrow(dat.c)){ #1:960 for all
  
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

coefs<-coefs; coefs[coefs<(-10)]<-NA
fit<-doesitfit

dat.check.coef<-as.data.frame(as.factor(dat.check$set_id)); dat.check.coef$k<-coefs; dat.check.coef$fit<-fit; colnames(dat.check.coef)[1]<-"line"
par(mfrow=c(1,1))

boxplot(data=dat.check.coef, k~line)
kext<-aov(data=dat.check.coef,k~line)
summary(kext)
TukeyHSD(kext)

# library(ggplot2)
# ggplot(dat.check.coef) +
#   aes(x = line, fill = fit) +
#   geom_bar() +
#   scale_fill_hue() +
#   theme_minimal()
# 
# par(mfrow=c(1,2))
# plot(density(propsat, na.rm=TRUE), main='% saturated', xlab="Proportion plant height in saturating sun",
#      lwd=2, font.axis=2, font.lab=2)
# plot(density(coefs[fit=="E"&!is.na(coefs)]), main='coefficients', xlim=c(-30,0), xlab="Value of exponential decay (e^kx) coefficient k",
#      lwd=2, font.axis=2, font.lab=2, na.rm=TRUE)
# 
# 
# #Make a sheet with all the data to be used elsewhere, e.g. with lodging
# colnames(dat.scale)<-c("L6", "L5", "L4", "L3", "L2", "L1")
# dat.print<-cbind(dat.lp[c(1:8)], dat.scale, dat.lp[15:32])
# colnames(dat.print)[ncol(dat.print)]<-"Flood"
# 

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

standard.ht<-extract.canval(dat.h=(1-height.scale), dat.l=dat.scale, pct=pct)
for(i in 1:nrow(standard.ht)){
plot(as.numeric(dat.scale[i,])~as.numeric(height.scale[i,]), type='l')
points(rev(as.numeric(standard.ht[i,]))~rev(1-pct))
}


par(mar=c(4,4,3,2))

dat.scale.doy<-dat.scale; dat.check.doy<-dat.check; height.scale.doy<-height.scale
linemeans<-heightmeans<-linesd<-data.frame(matrix(nrow=6, ncol=6))
linemeans.alt<-standsd<-data.frame(matrix(nrow=6, ncol=length(pct)))
linelist<-sort(unique(dat.check.doy$set_id))

par(mfrow=c(3,2))
gg<-list()

for(i in (1:length(linelist))){ #1:960 for all


  linedat<-dat.scale.doy[dat.check.doy$set_id==linelist[i],]; heightdat<-height.scale.doy[dat.check.doy$set_id==linelist[i],]
  stdat<-standard.ht[dat.check.doy$set_id==linelist[i],]; colnames(stdat)<-rev(pct)

  plot(as.numeric(dat.scale.doy[1,])~(as.numeric(height.scale.doy[1,])), xlab='Depth (scaled) ', ylab='Light (scaled)', pch=19,cex=1.5, xlim=c(0, 1), main=paste("line", linelist[i]), col='white')

  for(p in 1:nrow(linedat)){

    points(as.numeric(linedat[p,])~(as.numeric(heightdat[p,])), pch=19, col=i)
    #lines(as.numeric(linedat[p,])~(as.numeric(heightdat[p,])),lty=2, col=i)
    #points(rev(as.numeric(stdat[p,]))~rev(1-pct), pch=3)

    }

  lines(colMeans(rev(stdat))~rev(1-pct), lwd=3, col=i)
  linemeans.alt[i,]<-as.numeric(colMeans(rev(stdat)))
  standsd[i,]<-apply(stdat, 2, sd)
  
  #lines(colMeans(linedat,na.rm=TRUE)~colMeans(heightdat, na.rm=TRUE), lwd=3, col=i)
  
  linemeans[i,]<-colMeans(linedat,na.rm=TRUE); heightmeans[i,]<-colMeans(heightdat,na.rm=TRUE)
  linesd[i,]<-apply(linedat, 2, sd)
  
  #arrows(x0=as.numeric(heightmeans[i,]), x1=as.numeric(heightmeans[i,]), y0=as.numeric(linemeans[i,])-as.numeric(linesd[i,]),y1=as.numeric(linemeans[i,])+as.numeric(linesd[i,]), angle=90,length=0.1, code=3, col=i)
  arrows(x0=1-rev(pct), x1=1-rev(pct), y0=as.numeric(linemeans.alt[i,])-as.numeric(standsd[i,]),y1=as.numeric(linemeans.alt[i,])+as.numeric(standsd[i,]), angle=90,length=0.1, code=3, col=i)
  
}


par(mfrow=c(1,1))
plot(as.numeric(linemeans[1,])~as.numeric(heightmeans[1,]), col='white',  ylim=c(0,1), xlim=c(0,1), xlab='Depth (scaled) ', ylab='Light (scaled)')
for(i in c(1:6)){
#lines(as.numeric(linemeans[i,])~as.numeric(heightmeans[i,]), col=i, lwd=3)
lines(as.numeric(linemeans.alt[i,])~rev(1-pct),  col=i, lwd=3)
addon<-sample(seq(from=-0.05, to=0.05, by=0.01),1)
arrows(x0=1-rev(pct)+addon, x1=1-rev(pct)+addon, y0=as.numeric(linemeans.alt[i,])-as.numeric(standsd[i,]),y1=as.numeric(linemeans.alt[i,])+as.numeric(standsd[i,]), angle=90,length=0.1, code=3, col=i)
}
legend(0.7, 1, legend=paste("line",linelist[c(1:6)]), lwd=2, col=c(1:6))



