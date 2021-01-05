#I wonder about variability in width...
library('esquisse')
doc<-"Stem Width - Sheet1.csv"
  #"Heights_SP.csv"
  #"Stem Width - Sheet1.csv"


dat<-read.csv(doc)

dat.es<-dat[dat$SPECIES=="ES",]#(dat$SPECIES=="M"|dat$SPECIES=="ZM")

if(voi=="width"){
means<-data.frame(matrix(nrow=5, ncol=5));
par(mfrow=c(2,3), mar=c(4,4,4,1))
for(i in 1:5){
  boxplot(WIDTH~OBSERVER,data = droplevels(dat.es[dat.es$PLOT==i,]), main=paste("ES Plot",i), ylim=c(5,33))
  means[i,]<-(aggregate(dat.es$WIDTH[dat.es$PLOT==i], by=list(dat.es$OBSERVER[dat.es$PLOT==i]), FUN='mean')$x)
}

obs.df<-data.frame(t(means)); colnames(obs.df)<-c('Plot 1','Plot 2','Plot 3','Plot 4','Plot 5')

#par(mfrow=c(1,1))
boxplot(obs.df, main='OBSERVER MEANS', ylab='WIDTH', ylim=c(5,33), xlab='PLOT')
}


esquisser()

robo<-read.csv('Robodat.csv', skip=1)[,c(1,3:12)]
colnames(robo)<-toupper(colnames(robo)); colnames(robo)[colnames(robo)=="ROBO_WIDTH"]<-"WIDTH";colnames(robo)[colnames(robo)=="PLOT_ID"]<-"PLOT";colnames(robo)[colnames(robo)=="ROBO_HEIGHT"]<-"HEIGHT"
robo$OBSERVER<-c("RB1", "RB2", "RB3")
robo$AM<-"AUT"

combo<-merge(robo, dat.es, by=c("OBSERVER", "SPECIES", "PLOT","DATE","WIDTH"), all = TRUE) #"DATE","WIDTH","HEIGHT"
combo$AM[is.na(combo$AM)]<-'MAN'


col<-c(rep('dark gray', 4), 'red', 'dark gray')
means<-data.frame(matrix(nrow=5, ncol=8));
par(mfrow=c(2,3), mar=c(4,4,4,1))
for(i in 1:5){
  boxplot(WIDTH~OBSERVER,data = droplevels(combo[combo$PLOT==i,]), main=paste("ES Plot",i), ylim=c(5,33), col=col)
  means[i,]<-(aggregate(combo$WIDTH[combo$PLOT==i], by=list(combo$OBSERVER[combo$PLOT==i]), FUN='mean')$x)
}
z<-aggregate(combo, by=list(combo$OBSERVER,combo$PLOT), FUN='mean')


plotdat<-z[,c(1:2, 7,15)]; colnames(plotdat)[1:2]<-c("OBSERVER", "PLOT");plotdat$PLOT<-factor(plotdat$PLOT)
plotdat$AM<-"MAN"
plotdat$AM[which(plotdat$OBSERVER%in%c("RB1","RB2","RB3"))]<-"AUT"
plotdat$AM<-factor(plotdat$AM);plotdat<-plotdat[plotdat$OBSERVER!="Ed",]

plotdat.h<-combo[,c(1:4, 13)]; #plotdat.h<-plotdat.h[plotdat.h$OBSERVER!="Matt",]
plotdat.h$AM<-factor(plotdat.h$AM);plotdat.h$PLOT<-factor(plotdat.h$PLOT)



library(ggplot2)

ggplot(plotdat) +
 aes(x = PLOT, y = WIDTH, fill = AM) +
 geom_boxplot() +
 scale_fill_hue() +
 labs(x = "PLOT#", y = "WIDTH", title = "Stem width by AUTonomous and MANual observers", fill = "Autonomous or manual?") +
 theme_minimal() +
 theme(legend.position = "bottom")


ggplot(plotdat.h) +
  aes(x = PLOT, y = HEIGHT, fill = AM) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "PLOT#", y = "HEIGHT", title = "Height by AUTonomous and MANual observers", fill = "Autonomous or manual?") +
  theme_minimal()

