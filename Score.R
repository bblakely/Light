#Function to 'score' plots on light interception
source('AddInfo_LP.R')

dat.c<-dat.lp[,4:9]
#pseudocode/brainstorming:

#Start with the row before there's some change (where the leaves start)
pres<-200 #W/m2 attenuation that indicates a layer has leaves.
maxq<-1000 #W/m2 at which PS saturates
burnq<-2000 #W/m2 at which photodamage occurs

#Proportion of full sunlight that =maxq
#Used instead of absolute values because incoming sun varies throughout the day
satval<-maxq/quantile(dat.c$PPF_above_Avg, 0.95)

for(i in sample(1:nrow(dat.c), 5)){

  #Extract plot, graph it
  plot<-as.numeric(dat.c[i,])
  plot(plot, main=i, ylim=c(0,2300), xlim=c(1,6), ylab="PS light (Wm^2)", xlab="'height' (cart layer)")
  
  
  #Calcualte light absorbed at each level. e.g. S6 - S5 = light absorbed by layer 5
  atten<-diff(plot)
  
  ##Correcting for sensors brighter than ones above them ####

  #If S6 is below S5, set both to the highest value. Temporary solution.
  if(atten[5]<0){plot[5:6]<-max(plot[5:6]);points(plot, col='blue')}
  
  atten<-diff(plot) #reset atten after correction

  #If any other lower levels are brighter than higher levels, set both to the mean of the two
  err<-which(atten<0) #Finds the lower layer with the too-high value
  if(length(err)!=0){plot[c(err, err+1)]<-mean(c(plot[err], plot[err+1]));points(plot, col='red')} #Set it to the mean and add correceted points
  
  #Add line to fully corrected plot
  lines(plot, lty=2)
  

  atten<-diff(plot) #reset atten after all corrections

  #####
  
  ##Finding ei ####
  sum(atten) #Total am't light absorbed across all layers
  ei<-sum(atten)/max(plot);text(2,2300, paste("ei:", round(ei, 3))) #interception efficiency
  #####
  
  ##Account for light saturation. How much light is each layer usefully absorbing?####
  #Assumes curves would scale up with sun, i.e. a layer with 45% of max 5pm sun would get 45% of max 12pm sun
  #This assumption is not entirely suported because solar angle is different throughout the day
  
  abline(h=max(plot*satval), lty=3)#Add saturation line to graph
  
  plot.rel<-plot/max(plot) #proportion full sun at each layer
  plot.eff<-plot.rel/satval#proportion of saturated sun (i.e proportion of 46% full sun) each layer gets
  plot.eff<-plot.eff[2:6] # The first value is light that gets below 7in. Assumes  that this is the ground, i.e. no more leaves below 7in
  plot.eff[plot.eff>1]<-1 #When light is above saturation, reset to 1x light saturated value
  #Could add something to penalize for photodamage at high light. 
  
  #####
  
  ##Remove layers where there are clearly not leaves####
  defleaf<-max(which(atten>pres))+1 #Finds tallest layer that attenuates enough light for me to believe there are leaves there
  if(defleaf!=6){plot.eff[defleaf:5]<-0}#If the first layer isn't the top one, set the score of 'layers' above that (i.e. empty layers) to zero
  points(defleaf, plot[defleaf], pch=2, cex=2) 
  
  
  #Add 'stairsteps' that better represent values used for scoring
  x0h<-seq(0, 6, by=1);x1h<-seq(1, 7, by=1);
  segments(x0=x0h[1:defleaf], x1=x1h[1:defleaf], y0=plot[1:defleaf], y1=plot[1:defleaf], lwd=2)
  segments(x0=x1h[1:(defleaf-1)], x1=x1h[1:(defleaf-1)], y0=plot[1:(defleaf-1)], y1=plot[2:defleaf], lwd=2)
  
  #something is wrong here
  plot.score<-sum(plot.eff) #Number of layers doing light-saturated PS
  
  text(2,2100,paste("# light saturated layers:", round(plot.score, 2)))
  
  
}

