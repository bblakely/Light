lidat.raw<-read.csv('LAI_1819_FullSeason.csv', stringsAsFactors = FALSE)
colnames(lidat.raw)[colnames(lidat.raw)=='first_row']<-'row'

hidat.raw<-read.csv('Height_1819_FullSeason.csv', stringsAsFactors = FALSE)
colnames(hidat.raw)[colnames(hidat.raw)=='first_row']<-'row'


library(esquisse)
esquisser()

ggplot(dat.lp.bk) +
  aes(x = row, y = range, fill = height) +
  geom_tile(size = 1L) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal()

ggplot(dat.lp.bk) +
  aes(x = row, y = range, fill = lai) +
  geom_tile(size = 1L) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal()

ggplot(dat.lp.bk) +
  aes(x = row, y = range, fill = lai) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_minimal()


subdate.up<-function(dat,datecol='date'){ #Function may not work with different date formats!
  
  date.form<-as.Date(dat[,which(colnames(dat)==datecol)], format = "%m/%d/%Y")
  dat.ts<-as.POSIXct(date.form)
  dat$YEAR<-as.numeric(format(dat.ts, format= '%Y'))
  dat$DOY<-as.numeric(format(dat.ts, format= '%j'))
  rel<-dat#[which(dat$DOY%in%doy & dat$YEAR%in%year),] #DOY 211 = July 30, closest date to July 31 light measurement day
  
  return(rel)
}

lidat<-subdate.up(lidat.raw)
hidat<-subdate.up(hidat.raw)


prinplot<-function(dat){
  
years<-unique(dat$YEAR)
for(i in 1:length(years)){
  dat.year<-dat[dat$YEAR==years[i],]
  year.doys<-unique(dat.year$DOY)
  for(j in 1:length(year.doys)){
    
    subcode<-dat[dat$YEAR==years[i]&dat$DOY==year.doys[j],]
    
    p<-ggplot(subcode) +
      aes(x = row, y = range, fill = lai) +
      geom_tile(size = 1L) +
      scale_fill_distiller(palette = "Spectral", limits=c(0,10)) +
      labs(title = subcode$date) +
      xlim(0,150)+
      ylim(0,26)+
      theme_minimal()
    
    print(p)

  }
}
}

prinplot(lidat)

prinplot.h<-function(dat){
  
  years<-2019
  for(i in 1:length(years)){
    dat.year<-dat[dat$YEAR==years[i],]
    year.doys<-unique(dat.year$DOY)
    for(j in 1:length(year.doys)){
      
      subcode<-dat[dat$YEAR==years[i]&dat$DOY==year.doys[j],]
      
      p<-ggplot(subcode) +
        aes(x = row, y = range, fill = height) +
        geom_tile(size = 1L) +
        scale_fill_distiller(palette = "Spectral", limits=c(0,3.5)) +
        labs(title = subcode$date) +
        xlim(0,150)+
        ylim(0,26)+
        theme_minimal()
      
      print(p)
      
    }
  }
}

prinplot.h(hidat)

