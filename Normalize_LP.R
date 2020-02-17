#Script with parts from 'score' that scales light and height 0-1

source('AddInfo_LP.R') 

dat.c<-dat.lp[,which(colnames(dat.lp)%in%ppf.names)]



height.dat<-cliplp.dat<-data.frame(matrix(nrow=nrow(dat.c), ncol=6)) #Two df's of same size


for (i in (c(1:960))){
  
  plot<-as.numeric(dat.c[i,])
  
  ##QC####
  #Calcualte light absorbed at each level. e.g. S6 - S5 = light absorbed by layer 5
  atten<-diff(plot)
  
  #If S6 is below S5, set both to the highest value. Temporary solution.
  if(atten[5]<0){plot[5:6]<-max(plot[5:6])}
  
  atten<-diff(plot) #reset atten after correction
  
  #If any other lower levels are brighter than higher levels, set both to the mean of the two
  err<-which(atten<0) #Finds the lower layer with the too-high value
  if(length(err)!=0){plot[c(err, err+1)]<-mean(c(plot[err], plot[err+1]))} #Set it to the mean 
  #####
  
  ##Clip to actual height of plant####
  if(length(which(heights>dat.lp$height[i]*39.4))>1){ #If there is at least one measurement point above the known height of the plant...
    above<-which(heights>dat.lp$height[i]*39.4)[2:length(which(heights>dat.lp$height[i]*39.4))] #Points to discard. Starts at the second sensor above the measured height because the top of canopy is receiving the amount of sun measured by the first sensor above it
    plot.new<-plot;plot.new[above]<-NA #NaN sensor readings known to be above plant
  }else{plot.new<-plot}
  
  cliplp.dat[i,]<-plot.new #Sets new light values
  
  height.new<-c(heights[which(heights<dat.lp$height[i]*39.4)], dat.lp$height[i]*39.4) #Height of sensors below top of canopy + height at top of canopy
  length(height.new)<-6 #Will add NA's to vector until it's 6 long
  height.dat[i,]<-height.new #Set new height values; will be same length as light values
  #####

}


#Heights and light levels scaled: 
dat.rel<-data.frame(t(apply(cliplp.dat, 1, function(x) x/max(x, na.rm=TRUE))))
height.rel<-data.frame(t(apply(height.dat, 1, function(x) x/max(x, na.rm=TRUE))))


#Showing variability at similar LAI
center<-5; span.plot<-0.5; span<-0.02
laimin<-center-span.plot; laimax<- center+span.plot

laimin<-center-span; laimax<- center+span
lai.ind<-which(dat.lp$lai<laimax & dat.lp$lai>=laimin)
lai.subset<-cbind(dat.rel[lai.ind,], dat.lp$lai[lai.ind],  dat.lp$height[lai.ind])
height.subset<-height.rel[lai.ind,]
colnames(lai.subset)[7:8]<-c('lai', 'height')

height<-as.numeric(height.subset[1,1:6]) #First value of height, for initial plot

par(mfrow=c(4,4), mar=c(2,2,1,1))
for (i in sample(1:nrow(lai.subset), 16)){
  height<-as.numeric(height.subset[i,1:6]) #Reset to heights for each indiv. plot
  plot(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height),col=cols[sample(1:8,1)], type='l', lwd=2, lty=2, xaxt='n', ylab='', xlab='', font.axis=2, ylim=c(0,1))
  points(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height), pch=16)
  axis(side=1, labels=rev(seq(from=0, to=1, by=0.25)), at=(seq(from=0, to=1, by=0.25)), font.axis=2)
}


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

plotsamp<-sample(1:960, 27); plotsamp<-c(211,226)
#par(mfrow=c(3,3))
for(i in 200:230){ #1:960 for all
  
  if(i%in%plotsamp){
    #plot(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])), xlab='height', ylab='light', main=i)
    plot(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])), xlab='Depth (scaled) ', ylab='Light (scaled)', pch=19,cex=1.5, xlim=c(0, 1) )
    lines(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])), lty=2, col="#3A4919",lwd=2)
    #legend(0.75, 1,legend=c("observations",fiteq), lwd=c(NA,2), pch=c(19,NA), col=c( 'black','#7B883F'), bty='n', seg.len=1)
    }
  
  curve<-as.numeric(dat.scale[i,]);height.norm<-as.numeric(height.scale[i,])
  
  
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
  
  if(exists('AIClog')&exists('AICexp')& exists('sat.log')){
    if(AIClog<AICexp){
      doesitfit[i]<-'L'
      propsat[i]<-sat.log}
  }
  #if(i%in%plotsamp){text(0.05,0.2,doesitfit[i])}
  
  
  
  
  if(i %% 50 == 0){print(i)}
  rm('AICexp','AIClog')
  
}

length(which(doesitfit=='E'))/length(doesitfit)->prope
length(which(doesitfit=='L'))/length(doesitfit)->propl


par(mfrow=c(1,2))
plot(density(propsat, na.rm=TRUE), main='% saturated', xlab="Proportion plant height in saturating sun",
     lwd=2, font.axis=2, font.lab=2)
plot(density(coefs[doesitfit=="E"]), main='coefficients', xlim=c(-30,0), xlab="Value of exponential decay (e^kx) coefficient k",
     lwd=2, font.axis=2, font.lab=2)


#Make a sheet with all the data to be used elsewhere, e.g. with lodging


colnames(dat.scale)<-c("L6", "L5", "L4", "L3", "L2", "L1")
dat.print<-cbind(dat.lp[c(1:7)], dat.scale, dat.lp[14:31])
colnames(dat.print)[ncol(dat.print)]<-"Flood"

#write.csv(dat.print, "D:/R/TERRA_09.csv", row.names=FALSE)
