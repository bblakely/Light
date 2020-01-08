


par(mfrow=c(1,2))

#LAI 3000
strat.raw<-read.csv("LeafArea_Strat.csv", stringsAsFactors = FALSE)
strat<-strat.raw[1:5,1:9]
plot(as.numeric(strat[i,6:9]), ylim=c(2500,24000), col='white', main= "LAI3000 cumulative", xlab='"height"', ylab='leaf area')
for(i in 1:5){

lines(cumsum(as.numeric(strat[i,6:9])), col=i)
}
legend(1,24000,c(1:5), lwd=1, col=c(1:5), title="plot")


plot(as.numeric(strat[i,6:9]), ylim=c(2500,10000), col='white', main= "LAI3000 profile", xlab='"height"', ylab='leaf area')
for(i in 1:5){
  
  lines((as.numeric(strat[i,6:9])), col=i)
}

#LAI meter
lai.raw<-read.csv("LAI2200_Strat.csv")
lai<-lai.raw[,4:7]
laiprof<-matrix(nrow=nrow(lai), ncol=ncol(lai))

plot(1:4, col='white', ylim=c(0,6), main="LAI2000 profile")
for(i in (1:5)){
laiprof[i,]<-c(rev(diff(rev(unlist(lai[i,])))), unlist(lai[i,4])); names(laiprof)[4]<-"LAI_LP4"

dats<-unlist(laiprof[i,]); dats[dats==0]<-NA
lines(cumsum(dats), col=i)

}
laiprof<-data.frame(laiprof); colnames(laiprof)<-colnames(lai)
legend(1,6,c(1:5), lwd=1, col=c(1:5), title="plot")

