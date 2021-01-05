#Run normalizeLP before this


extract.canval<-function(dat.h, dat.l, pct=0.5, res=20){
  holder<-rep(-1, nrow(dat.l))
  
  for(i in 1:nrow(dat.l)){
    
    heightstr<-approx(as.numeric(dat.h[i,]), n=res)$y
    datstr<-approx(as.numeric(dat.l[i,]), n=res)$y
    
    ind<-which(abs(heightstr-pct)==min(abs(heightstr-pct)))
    at.pct<-datstr[ind]
    
    holder[i]<-at.pct
  }
  
  return(holder)
}

pcts<-extract.canval(dat.h=height.rel, dat.l=dat.rel, res=100)

plotdat.ph<-cbind(dat.lp, pcts)

#library(esquisse)
#esquisser()

library(ggplot2)
ggplot(plotdat.ph) +
  aes(x = row, y = range, fill = pcts) +
  geom_tile(size = 1L) +
  scale_fill_gradientn(colours=c("#3A4919","#7B883F","Lightgoldenrod1","white")) +
  theme_minimal()




