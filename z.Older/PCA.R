testcols<-c(21:25, 28:31)
par(mfrow=c(3,3))
dat<-dat.print[testcols]
dat[,9]<-as.numeric(dat[,9])
nncount<-rep(0,10)
for (i in(1:ncol(dat))){
  
  plot(density(dat[,i], na.rm=TRUE), main=colnames(dat)[i])
  nncount[i]<-shapiro.test(dat[,i])[[2]]
}


dat.clean<-na.omit(dat)
library(corrplot)


#Do you want to remove flood zone plots?
loseflood<-T
loselodge<-T

if(loseflood==T){dat1<-dat.clean[dat.clean$Flood==0,]}else{dat1<-dat.clean}
if(loselodge==T){dat1<-dat1[dat1$Score<4,]}else{dat1<-dat1}
#all the data
pass1<-princomp(dat1, cor=TRUE, scores=TRUE)
summary(pass1)

#All useful data  columns
#usecol<-c(1:5, 8:11)
if(loseflood==TRUE){dat2<-dat1[,1:8]}else(dat2<-dat1)#[usecol]
corrplot(cor(dat2), method='color', type='upper',addCoef.col = 'black');
pass2<-princomp(dat2, cor=TRUE, scores=TRUE)
summary(pass2)
loadings(pass2)

#Numeric variables (removes flood designation)
numcol<-c(1:4, 6:8)
dat3<-dat2[numcol]
corrplot(cor(dat3), method='shade', type='upper')
pass3<-princomp(dat3, cor=TRUE, scores=TRUE)
summary(pass3)

#Variables related to the plant itself
plantcol<-c(1:4, 6:7)
dat4<-dat3[plantcol]
corrplot(cor(dat4), method='shade', type='upper')
pass4<-princomp(dat4, cor=TRUE, scores=TRUE)
summary(pass4)
loadings(pass4)
biplot(pass4)

#Remove  yield (it doesn't do much) and NIR albedo (tracks VIS albedo)
#This one is worth playing with. Keeping yield in but removing NIR makes yield track VIS
#Keeping NIR reduces the loadings of PC2
strcol<-c(1:3, 5:6)
dat5<-dat4[,strcol]
corrplot(cor(dat5), method='shade', type='upper')
pass5<-princomp(dat5, cor=TRUE, scores=TRUE)
summary(pass5)
loadings(pass5)
biplot(pass5)
score5<-pass5$scores

#Just height, lai, row density
strcol<-c(1:3)
dat6<-dat5[,strcol]
corrplot(cor(dat6), method='shade', type='upper')
pass6<-princomp(dat6, cor=TRUE, scores=TRUE)
summary(pass6)
loadings(pass6)
biplot(pass6)

#Height, lai, row density, yield
strcol<-c(1:4)
dat7<-dat4[,strcol]
corrplot(cor(dat7), method='shade', type='upper')
pass7<-princomp(dat7, cor=TRUE, scores=TRUE)
summary(pass7)
loadings(pass7)
biplot(pass7)

height<-quantile(dat.print$height, c(0.2, 0.8))
lai<-quantile(dat.print$lai, c(0.2, 0.8))
cat<-rep(0,nrow(dat.print))
for (i in 1:nrow(dat.print)){
  
if(dat.print$height[i]<height[1]& dat.print$lai[i]<lai[1]){cat[i]<-'st'}else 
  if(dat.print$height[i]>height[2]&dat.print$lai[i]<lai[1]){cat[i]<-'tt'}else
    if(dat.print$height[i]<height[1]&dat.print$lai[i]>lai[2]){cat[i]<-'sb'}else
      if(dat.print$height[i]>height[2]&dat.print$lai[i]>lai[2]){cat[i]<-'tb'}else
       if(dat.print$height[i]<height[1]&dat.print$lai[i]>lai[1]&dat.print$lai[i]<lai[2]){cat[i]<-'sm'}else
        if(dat.print$height[i]>height[2]&dat.print$lai[i]>lai[1]&dat.print$lai[i]<lai[2]){cat[i]<-'tm'}else{cat[i]<-'mid'}
  
}



dat.print$cat<-cat
dat.print$proxwidth<-dat.print$above_ground_dry_yield/dat.print$height

seedz<-read.csv('Sorghum_Seed.csv');seedz$genotype_name<-paste(seedz$CAT, seedz$ID, sep='')
dat.print.a<-merge(dat.print, seedz, by='genotype_name', sort=FALSE, all=FALSE) #Add plot info
dat.print.a<-dat.print.a[dat.print.a$Amount>3,]

# shorthin<-dat.print.a[which(dat.print.a$Flood==0&dat.print.a$cat=='st'),];stgeno<-as.character(unique(shorthin$genotype_name))
# shortbushy<-dat.print.a[which(dat.print.a$Flood==0&dat.print.a$cat=='sb'),];sbgeno<-as.character(unique(shortbushy$genotype_name))
# tallthin<-dat.print.a[which(dat.print.a$Flood==0&dat.print.a$cat=='tt'),];ttgeno<-as.character(unique(shortbushy$genotype_name))
# tallbushy<-dat.print.a[which(dat.print.a$Flood==0&dat.print.a$cat=='tb'),];tbgeno<-as.character(unique(shortbushy$genotype_name))
# 
# shortmed<-dat.print.a[which(dat.print.a$Flood==0&dat.print.a$cat=='sm'),]; smgeno<-as.character(unique(shortmed$genotype_name))
# tallmed<-dat.print.a[which(dat.print.a$Flood==0&dat.print.a$cat=='tm'),];tmgeno<-as.character(unique(tallmed$genotype_name))

dat.print.a<-data.frame(dat.print.a[!duplicated(dat.print.a[,c('genotype_name')]),])



#Okay new plan yield per height
dat.print.a %>%
  filter(lai >= 2.5 & lai <= 7.5) %>%
  ggplot() +
  aes(x = height, y = proxwidth, colour = lai) +
  geom_point(size = 1.5) +
  scale_color_distiller(palette = "Spectral") +
  theme_minimal() +
  facet_wrap(vars(Flood))


thicc<-quantile(dat.print.a$proxwidth, c(0.1,0.9), na.rm=TRUE)
thickstems<-dat.print.a[dat.print.a$proxwidth>thicc[2],]
thinstems<-dat.print.a[dat.print.a$proxwidth<thicc[1],]

write.csv(thickstems[,c(1:4, 20:26,29,32:37,40:42)], 'Thickstems.csv')
write.csv(thinstems[,c(1:4, 20:26,29,32:37,40:42)], 'Thinstems.csv')


#Picking lines r2

shorts<-dat.print.a[dat.print.a$height<height[1],c(1:3, 20:41)]
talls<-dat.print.a[dat.print.a$height>height[2],c(1:3, 20:41)]

sd(shorts$lai);sd(talls$lai)

brg<-c(mean(talls$lai)+1*(sd(talls$lai)), mean(talls$lai)+2*(sd(talls$lai)))
trg<-c(mean(talls$lai)-1*(sd(talls$lai)), mean(talls$lai)-2*(sd(talls$lai)))

tbset<-talls[which(talls$lai>brg[1]&talls$lai<brg[2]),]
ttset<-talls[which(talls$lai<trg[1]&talls$lai>trg[2]),]

brg<-c(mean(shorts$lai)+1*(sd(shorts$lai)), mean(shorts$lai)+2*(sd(shorts$lai)))
trg<-c(mean(shorts$lai)-1*(sd(shorts$lai)), mean(shorts$lai)-2*(sd(shorts$lai)))

sbset<-shorts[which(shorts$lai>brg[1]&shorts$lai<brg[2]),]
stset<-shorts[which(shorts$lai<trg[1]&shorts$lai>trg[2]),]

#Function to thin sets by only taking those with medium values for other vars
thinpicks<-function(dat){
  dat<-dat[dat$Flood==0 &dat$Score <3,]
  
  hist(dat$row_density)
  rdrg<-c(mean(dat$row_density)-sd(dat$row_density), mean(dat$row_density)+sd(dat$row_density))
  
  hist(dat$above_ground_dry_yield)
  ylrg<-c(mean(dat$above_ground_dry_yield, na.rm=TRUE)-sd(dat$above_ground_dry_yield, na.rm=TRUE), mean(dat$above_ground_dry_yield, na.rm=TRUE)+sd(dat$above_ground_dry_yield, na.rm=TRUE))
  
  # hist(dat$vis.400.700)
  # vsrg<-c(mean(dat$vis.400.700)-2*sd(dat$vis.400.700), mean(dat$vis.400.700)+2*sd(dat$vis.400.700))
  #   
  # hist(dat$nir.700.1000)
  # nirg<-c(mean(dat$nir.700.1000)-2*sd(dat$nir.700.1000), mean(dat$nir.700.1000)+2*sd(dat$nir.700.1000))
  
  dat.fin<-dat[which(dat$row_density>rdrg[1]&dat$row_density<rdrg[2] & dat$above_ground_dry_yield>ylrg[1]&dat$above_ground_dry_yield<ylrg[2]),]

  return(dat.fin)
  
}


tbselect<-thinpicks(tbset)
tbnodup<-tbselect[!duplicated(tbselect[,c('genotype_name')]),]
#Best choice:

ttselect<-thinpicks(ttset)
ttnodup<-ttselect[!duplicated(ttselect[,c('genotype_name')]),]

sbselect<-thinpicks(sbset)
sbnodup<-sbselect[!duplicated(sbselect[,c('genotype_name')]),]
#Best choice

stselect<-thinpicks(stset)
stnodup<-stselect[!duplicated(stselect[,c('genotype_name')]),]

#a medium line
mids<-dat.print.a[dat.print.a$cat=="mid",c(1:3, 20:41)]

thinpicks.med<-function(dat, mult=1){
  dat<-na.omit(dat)
  dat<-dat[dat$Flood==0 &dat$Score <3,]
  
  hist(dat$row_density)
  rdrg<-c(mean(dat$row_density)-mult*sd(dat$row_density), mean(dat$row_density)+mult*sd(dat$row_density))
  
  hist(dat$above_ground_dry_yield)
  ylrg<-c(mean(dat$above_ground_dry_yield)-mult*sd(dat$above_ground_dry_yield), mean(dat$above_ground_dry_yield)+mult*sd(dat$above_ground_dry_yield))
  
  hist(dat$lai)
  larg<-c(mean(dat$lai)-mult*sd(dat$lai), mean(dat$lai)+mult*sd(dat$lai))
  
  hist(dat$height)
  htrg<-c(mean(dat$height)-mult*sd(dat$height), mean(dat$height)+mult*sd(dat$height))
  
  # hist(dat$vis.400.700)
  # vsrg<-c(mean(dat$vis.400.700)-2*sd(dat$vis.400.700), mean(dat$vis.400.700)+2*sd(dat$vis.400.700))
  #   
  # hist(dat$nir.700.1000)
  # nirg<-c(mean(dat$nir.700.1000)-2*sd(dat$nir.700.1000), mean(dat$nir.700.1000)+2*sd(dat$nir.700.1000))
  
  dat.fin<-dat[which(dat$row_density>rdrg[1]&dat$row_density<rdrg[2] & 
                       dat$above_ground_dry_yield>ylrg[1]&dat$above_ground_dry_yield<ylrg[2] & 
                       dat$lai>larg[1]&dat$lai<larg[2] & 
                       dat$height>htrg[1]&dat$height<htrg[2]),]
                        
  return(dat.fin)
  
}

medselect<-thinpicks.med(mids, mult=0.6)
mnodup<-medselect[!duplicated(medselect[,c('genotype_name')]),]


