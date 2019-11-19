#Function to 'score' plots on light interception
source('AddInfo_LP.R')

dat.c<-dat.lp[,8:13]


#Start with the row before there's some change (where the leaves start)
pres<-200 #W/m2 attenuation that indicates a layer has leaves.
maxq<-1000 #W/m2 at which PS saturates
burnq<-2000 #W/m2 at which photodamage occurs

#Proportion of full sunlight that = maxq
#Used instead of absolute values because incoming sun varies throughout the day
satval<-maxq/quantile(dat.c$PPF_above_Avg, 0.95)

#Dataframe for storing loop results
score.df<-data.frame(matrix(nrow=nrow(dat.c), ncol=2));colnames(score.df)<-c('ei','satlayer')# Currently two, light sat layers and ei

#Dataframe for 'clean' profiles
dat.qc<-data.frame(matrix(nrow=nrow(dat.c), ncol=ncol(dat.c)));colnames(dat.qc)<-colnames(dat.c)

#Big loop that (1) cleans each plot and saves the clean dataset (2) calculates Ei (3) attempts to score curve by how many layers are light-saturated
#This one does NOT rescale the light or height but instead clips cart layers above pplant height and sets saturating sun at a fixed percent of above-canopy sun at the time of measurement

for(i in (1:nrow(dat.c))){

  #Extract plot, graph it
  plot<-as.numeric(dat.c[i,])
  plot(plot~heights, main=i, ylim=c(0,2300), xlim=c(0,90), ylab="PS light (Wm^2)", xlab="'height' (cart layer)")
  
  
  #Calcualte light absorbed at each level. e.g. S6 - S5 = light absorbed by layer 5
  atten<-diff(plot)
  
  ##Correcting for sensors brighter than ones above them ####

  #If S6 is below S5, set both to the highest value. Temporary solution.
  if(atten[5]<0){plot[5:6]<-max(plot[5:6]);points(plot~heights, col='blue')}
  
  atten<-diff(plot) #reset atten after correction

  #If any other lower levels are brighter than higher levels, set both to the mean of the two
  err<-which(atten<0) #Finds the lower layer with the too-high value
  if(length(err)!=0){plot[c(err, err+1)]<-mean(c(plot[err], plot[err+1]));points(plot~heights, col='red')} #Set it to the mean and add correceted points
  
  #Add line to fully corrected plot
  lines(plot~heights, lty=2)
  
  dat.qc[i,]<-plot

  atten<-diff(plot) #reset atten after all corrections

  #####
  
  ##Finding ei ####
  sum(atten) #Total am't light absorbed across all layers
  ei<-sum(atten)/max(plot);text(5,2300, paste("ei:", round(ei, 3))) #interception efficiency
  
  score.df$ei[i]<-ei
  #####
  
  ##Account for light saturation. How much light is each layer usefully absorbing?####
  #Assumes curves would scale up with sun, i.e. a layer with 45% of max 5pm sun would get 45% of max 12pm sun
  
  abline(h=max(plot*satval), lty=3)#Add saturation line to graph
  
  plot.rel<-plot/max(plot) #proportion full sun at each layer
  plot.eff<-plot.rel/satval #proportion of saturated sun (i.e proportion of 46% full sun) each layer gets
  plot.eff<-plot.eff[2:6] # The first value is light that gets below 7in. Assumes  that this is the ground, i.e. no more leaves below 7in
  plot.eff[plot.eff>1]<-1 #When light is above saturation, reset to 1x light saturated value
  #Could add something to penalize for photodamage at high light. 
  
  #####
  
  ##Remove layers where there are clearly not leaves####
  
  if(any(atten>pres)==TRUE){
  defleaf<-max(which(atten>pres))+1 #Finds tallest layer that attenuates enough light for me to believe there are leaves there
  if(defleaf!=6){plot.eff[defleaf:5]<-0}#If the first layer isn't the top one, set the score of 'layers' above that (i.e. empty layers) to zero
  points(heights[defleaf], plot[defleaf], pch=2, cex=2) 
  
  #Add 'stairsteps' that better represent values used for scoring
  x0h<-c(0, heights);x1h<-c(heights, 90)
  
  segments(x0=x0h[1:defleaf], x1=x1h[1:defleaf], y0=plot[1:defleaf], y1=plot[1:defleaf], lwd=2)
  segments(x0=x1h[1:(defleaf-1)], x1=x1h[1:(defleaf-1)], y0=plot[1:(defleaf-1)], y1=plot[2:defleaf], lwd=2)
  
  }else{print(paste("apparently no leaves in plot", i))}
  
  #####
  
  
  abline(v=dat.lp$height[i]*39.4, col='purple', lty=2) #39.4 converts meters to inches
  
  #something is wrong here
  plot.score<-sum(plot.eff) #Number of layers doing light-saturated PS
  score.df$satlayer[i]<-plot.score
  
  text(20,2100,paste("# light saturated layers:", round(plot.score, 2)))
  
}

#Explore curvefitting#####
curve.raw<-as.numeric(dat.c[sample(1:nrow(dat.c),1),])

#Function to do corrections on a single plot
cleanleaf<-function(plot, pres){
  
  atten<-diff(plot)
  
  #If S6 is below S5, set both to the highest value. Temporary solution.
  if(atten[5]<0){plot[5:6]<-max(plot[5:6]);points(plot~heights, col='blue')}
  
  atten<-diff(plot) #reset atten after correction
  
  #If any other lower levels are brighter than higher levels, set both to the mean of the two
  err<-which(atten<0) #Finds the lower layer with the too-high value
  if(length(err)!=0){plot[c(err, err+1)]<-mean(c(plot[err], plot[err+1]));points(plot~heights, col='red')} #Set it to the mean and add correceted points
  
  atten<-diff(plot) #reset atten after correction
  
  #If there is effectively no attenuation in high layers, there are no leaves there
  defleaf<-max(which(atten>pres))+1 #Finds tallest layer that attenuates enough light for me to believe there are leaves there
  if(defleaf!=6){plot[(defleaf+1):6]<-0}#If the first layer isn't the top one, set the score of 'layers' above that (i.e. empty layers) to zero

  return(plot)  
  }

plot(curve.raw~heights)
cleanleaf(curve.raw, pres)
curve<-cleanleaf(curve.raw, pres)
curve[curve==0]<-NA
points(curve~heights, pch=19)

#This bit uses predict incorrectly but I'm not ready to trash it

fake<-seq(from=min(heights[which(!is.na(curve))]),to=max(heights[which(!is.na(curve))]), length.out=length(which(!is.na(curve))))

fit1.d3<-lm(curve~poly(heights,3,raw=TRUE)) #quadratic, orange
lines(fit1.d3$fitted.values~fake, col='orange')

fit2<-nls(curve~SSlogis(heights, Asym,xmid,scal)) #logistic, blue
if(exists('fit2')){
  lines(fake, predict(fit2, data.frame(x=fake)), col='blue')
  rm(fit2)}else{print("Log fit failed")}

fit3.b<-nls(curve ~ exp(k*heights+i), start=c(k=0.2, i=0)) #exponential, red
lines(fake, predict(fit3.b, data.frame(x=fake)), col='red')

fit4<-lm(curve~heights)
lines(fake, fit4$fitted.values, lty=2)

#####


#Round 2: adapt code to be normalized by height, sun
#Goals
#All plots range 0-1, i.e. bottom of plant to top of plant <-DONE
#New algorithm to score with scaled sunlight

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
  
  ##Reprocess for 0-1 heights####
  if(length(which(heights>dat.lp$height[i]*39.4))>1){
  above<-which(heights>dat.lp$height[i]*39.4)[2:length(which(heights>dat.lp$height[i]*39.4))] #Points to discard. Starts at the second sensor above the measured height because the top of canopy is receiving the amount of sun measured by the first sensor above it
  plot.new<-plot;plot.new[above]<-NA
  }else{plot.new<-plot}
  
  cliplp.dat[i,]<-plot.new
  
  height.new<-c(heights[which(heights<dat.lp$height[i]*39.4)], dat.lp$height[i]*39.4)
  length(height.new)<-6 #Will add NA's to vector until it's 6 long
  height.dat[i,]<-height.new
  #####
  
  height.norm<-height.new/max(height.new, na.rm=TRUE)
  plot(plot.new~height.norm, xlim=c(0,1.1), ylim=c(0,2300));lines(plot.new~height.norm, lty=2)
  
  #dat.qc[i,]<-plot.new
  
  #CURVE FITS####
  
  curve<-plot.new
  #heights<-height.norm
  
  fake<-data.frame(seq(from=0, to=1, by=0.05));colnames(fake)<-'height.norm'
  
  
  # fit1.d2<-lm(curve~poly(heights,2,raw=TRUE)) #quadratic, orange
  # lines(fit1.d2$fitted.values~fake, col='orange', lty=2)
  
  tryCatch({
  
  fit1.d3<-lm(curve~poly(height.norm,3,raw=TRUE)) #quadratic, orange
  #lines(height.norm[which(!is.na(height.norm))], predict(fit1.d3), col='orange', lty=2)
  lines(fake$height.norm, predict(fit1.d3, newdata=fake), col='orange')
  AICpoly<-round(AIC(fit1.d3), 3);text(0.2, 1200, paste("POLY= ",AICpoly), cex=0.8)
  }, error=function(e){print("Poly failed to fit")})
  
  
  tryCatch({
  fit2<-nls(curve~SSlogis(height.norm, Asym,xmid,scal)) #logistic, blue
  if(exists('fit2')){
    #lines(height.norm[which(!is.na(height.norm))], predict(fit2), col='blue', lty=2)
    lines(fake$height.norm, predict(fit2, newdata=fake), col='blue')
    AIClog<-round(AIC(fit2), 3);text(0.2, 1100, paste("LOG= ",AIClog), cex=0.8)
    rm('fit2')}
  }, error=function(e){
    #print("Log failed to fit")
    text(0.2, 1100, paste("LOG= ", "NOFIT"), cex=0.8)
    })
  
  
  # fit3.a<-nls(curve ~ (exp(k*heights)), start=c(k=0.2)) #exponential, red
  # lines(fake, predict(fit3.a, data.frame(x=fake)), col='red', lty=2)

  tryCatch({  
  fit3.b<-nls(curve ~ a*exp(k*height.norm), start=c(k=5, a=1)) #exponential, red
  if(exists('fit3.b')){
  #lines(height.norm[which(!is.na(height.norm))], predict(fit3.b), col='red', lty=2)
  lines(fake$height.norm, predict(fit3.b, newdata=fake), col='red')
  AICexp<-round(AIC(fit3.b), 3); text(0.2, 1000, paste("EXP= ", AICexp), cex=0.8)
  rm('fit3.b')}
  }, error=function(e){
    #print("Exp failed to fit")
    text(0.2, 1000, paste("EXP= ", "NOFIT"), cex=0.8)
    })
  


  fit4<-lm(curve~height.norm)
  if(exists('fit4')){
  #lines(height.norm[which(!is.na(height.norm))], predict(fit4))
  AIClin<-round(AIC(fit4), 3);text(0.2, 900, paste("LIN= ",AIClin), cex=0.8)
  #if(AIClin<AIClog & AIClin<AICexp){lines(fake$height.norm, predict(fit4, newdata=fake), col='black')}
  rm('fit4')}
  
  #}, error=function(e){print("one or more models failed to find a fit")})
  
  legend(0,2300,legend=c("polynomial", "exponential", "logistic", "linear"), lty=1, col=c('orange', 'red', 'blue','black'))
  text(0.2, 1300, "AIC:")
  
  #####
  
  
}


#Heights and light levels scaled: 

dat.rel<-data.frame(t(apply(cliplp.dat, 1, function(x) x/max(x, na.rm=TRUE))))
height.rel<-data.frame(t(apply(height.dat, 1, function(x) x/max(x, na.rm=TRUE))))



#Official sanity checks/plots for terra meeting####

#SCALED version (results in 0-1 for both sun level and height)
dat.rel.plot<-data.frame(t(apply(dat.qc, 1, function(x) x/max(x))))

#Plot relationship with LAI
par(mfrow=c(2,3), mar=c(4,4,4,1))
for(i in 1:6){
plot(dat.rel.plot[,i]~dat.lp$lai, ylim=c(0,1), pch=19, main=paste("At ", round(heights[i]/39.4, 2), "m", sep=''), xlab="Total LAI", ylab="Proportion full sun")
}


#Set LAI limits for further exploration
center<-5; span.plot<-0.5
laimin<-center-span.plot; laimax<- center+span.plot
#Plot with LAIs of interest highlighted
par(mfrow=c(2,3), mar=c(4,4,4,1))
for(i in 1:6){
  plot(dat.rel.plot[,i]~dat.lp$lai, ylim=c(0,1), pch=19, 
       main=paste("At ", round(heights[i], 2), "m", sep=''), xlab="Total LAI",
       ylab="Proportion full sun", cex=0.8)
  polygon(x=c(laimin,laimin,laimax,laimax), c(-0.2,1.2,1.2,-0.2), col=rgb(1,1,0,0.2))
  
}

#####

#Reset more narrow span for further analysis#####
span<-0.02; 
laimin<-center-span; laimax<- center+span
lai.ind<-which(dat.lp$lai<laimax & dat.lp$lai>=laimin)
lai.subset<-cbind(dat.rel[lai.ind,], dat.lp$lai[lai.ind],  dat.lp$height[lai.ind])
height.subset<-height.rel[lai.ind,]
colnames(lai.subset)[7:8]<-c('lai', 'height')

height<-as.numeric(height.subset[1,1:6]) #First value of height, for initial plot

#Plot with many curves at approx. the same LAI
cols<-brewer.pal(n=8, name='Dark2')

par(mfrow=c(1,1))
plot(rev(as.numeric(lai.subset[1,1:6]))~rev((max(height, na.rm=TRUE)-height)), type='l', xlim=c(0,1), ylim=c(0,1), ylab='Proportion of full sun',
     xlab='Proportion total plant height', main=paste('LAI =', center, "(+/-", span, ")"), 
     xaxt='n', col='white', font.lab=2, font.axis=2)
axis(side=1, labels=rev(seq(from=0, to=1, by=0.25)), at=(seq(from=0, to=1, by=0.25)), font.axis=2)
for (i in sample(1:nrow(lai.subset), 8)){
  height<-as.numeric(height.subset[i,1:6]) #Reset to heights for each indiv. plot
  lines(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height),col=cols[sample(1:8,1)], lwd=2, lty=2)
  points(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height), pch=16)
  }

#Same plot in multipanel form
par(mfrow=c(4,4), mar=c(2,2,1,1))
for (i in sample(1:nrow(lai.subset), 16)){
  height<-as.numeric(height.subset[i,1:6]) #Reset to heights for each indiv. plot
  plot(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height),col=cols[sample(1:8,1)], type='l', lwd=2, lty=2, xaxt='n', ylab='', xlab='', font.axis=2, ylim=c(0,1))
  points(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height), pch=16)
  axis(side=1, labels=rev(seq(from=0, to=1, by=0.25)), at=(seq(from=0, to=1, by=0.25)), font.axis=2)
}

#version to get just one (as example)
orig<-sample(1:nrow(lai.subset), 1) #19 is nice
example<-lai.subset[orig,]
ex.dat<-(rev(as.numeric(example[1:6]))); ex.height<-rev(as.numeric(max(height.subset[orig,], na.rm=TRUE)-height.subset[orig,]))
#points(ex.dat~ex.height, pch=3)

par(mfrow=c(1,1), mar=c(4,4,4,1))
plot(ex.dat~ex.height, ylim=c(0,1), pch=16, ylab='Light', xlab='Height', xlim=c(0,1), xaxt='n', font.lab=2, font.axis=2)
axis(side=1, at=seq(0, 1, by=0.2), labels=rev(seq(0, 1, by=0.2)), font=2)
#lines(ex.dat~ex.height, lty=2, lwd=2)
#text(0.75, 0.95, expression('('~I[h] ~'=' ~ e^kh~')'), cex=1.2)

fit<-nls(ex.dat ~ a*exp(k*ex.height), start=c(k=-1, a=1)) 
fake<-data.frame(seq(from=0, to=1, by=0.1)); colnames(fake)<-'ex.height'
#lines(ex.height[!is.na(ex.height)], predict(fit))
lines(fake$ex.height~predict(fit, newdata=fake), lwd=2)
#text(0.1, 1, paste("k =", round(coef(fit)[1], 2)))

# lai.subset<-cbind(dat.rel[lai.ind,], dat.lp$lai[lai.ind],  dat.lp$height[lai.ind])
# height.subset<-height.dat[lai.ind,]
# colnames(lai.subset)[7:8]<-c('lai', 'height')

# #unscaled heights
# height<-heights/39.4
# 
# par(mar=c(4,4,4,1), mfrow=c(1,1))
# 
# plot(rev(as.numeric(lai.subset[1,1:6]))~rev((max(height, na.rm=TRUE)-height)), type='l', xlim=c(0,2.3), ylim=c(0,1), ylab='Proportion of full sun',
#       xlab='measurement height', main=paste('Light attenuation for LAI of', center, "(+/-", span, ")"), 
#       xaxt='n')
#  axis(side=1, labels=rev(seq(from=0, to=2, by=0.5)), at=(seq(from=0, to=2, by=0.5)))
# 
#  
# for (i in 1:nrow(lai.subset)){
#   lines(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height),col=i)
#   points(rev(as.numeric(lai.subset[i, 1:6]))~rev(max(height, na.rm=TRUE)-height),col=i)
#   }

#####

 
 
 
 
#Bulk processing of scaled curves


dat.scale<-data.frame(t(apply(dat.rel, 1, function(x) rev(x)))) #LAI starting from the top
height.scale<-data.frame(t(apply(height.rel, 1, function(x) rev(as.numeric(max(x, na.rm=TRUE)-x))))) #height starting from the top

#set up axis for reuse

plot(c(1:10), ylim=c(0,1), xlim=c(0,1), col='white', xaxt='n')
axis(side=1, at=seq(0, 1, by=0.2), labels=rev(seq(0, 1, by=0.2)))

doesitfit<-rep('n', 960);propsat<-rep(1, 960)
fake<-data.frame(seq(from=0, to=1, by=0.05));colnames(fake)<-'height.norm' #For smooth plotting

sat.sun<-0.46 #Fraction full sun considered saturating

for(i in 1:960){
  plot(as.numeric(dat.scale[i,])~(as.numeric(height.scale[i,])))
  
  curve<-as.numeric(dat.scale[i,]);height.norm<-as.numeric(height.scale[i,])
  
  
  tryCatch({  
    fit3.b<-nls(curve ~ exp(k*height.norm), start=c(k=-1)) #exponential, red
    if(exists('fit3.b')){
      lines(fake$height.norm, predict(fit3.b, newdata=fake), col='red')
      AICexp<-round(AIC(fit3.b), 3); #text(0.2, 1000, paste("EXP= ", AICexp), cex=0.8)
      doesitfit[i]<-'E'
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
        lines(fake$height.norm, predict(fit3.c, newdata=fake), col='blue')
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
  text(0.05,0.2,doesitfit[i])
  
 
  
  
  if(i %% 50 == 0){print(i)}
  rm('AICexp','AIClog')
  
}
   
length(which(doesitfit=='E'))/length(doesitfit)
length(which(doesitfit=='L'))/length(doesitfit)

hist(propsat)#histogram of proportion canopy in saturating sun

