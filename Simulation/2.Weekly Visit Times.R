library(dplyr)
library(dbscan)
library(ggplot2)
Freq<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181101/5.FreqPlacesFreqNY_HW_Coord_Fill.csv', stringsAsFactors = FALSE, header=T)
Trip<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/1.Trip.csv', stringsAsFactors = FALSE, header=T)
UID0<-unique(Freq$UID)
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  x0<-data.frame(matrix(0, nrow=1, ncol=3))
  x0[1, 1]<-UID0[i]
  
  ### Freq Places
  a1<-filter(Freq, UID==UID0[i])
  a1<-a1[-which(a1$Name=='Home'), ]
  a1<-a1[-which(a1$Name=='Work'), ]
  
  b1<-filter(Trip, UID==UID0[i])
  b2<-filter(b1, CurPlace%in%a1$Name)
  
  c1<-unique(b2$WeekID)
  d<-0
  for(j in 1:length(c1)){
    d1<-filter(b2, WeekID==c1[j])
    d2<-nrow(d1)/length(unique(d1$CurPlace))
    d<-d+d2
  }
  x0[1, 2]<-d/length(c1)
  
  ### Unknown Places
  b2<-filter(b1, !CurPlace%in%c(a1$Name, 'Home', 'Work'))
  c1<-unique(b2$WeekID)
  d<-0
  for(j in 1:length(c1)){
    d1<-filter(b2, WeekID==c1[j])
    if(nrow(d1)>0){
      d2<-matrix(c(d1$Longitude, d1$Latitude), ncol=2, byrow=F)
      d3<-dbscan(d2, eps=0.001, minPts = 1, borderPoints = T, search='kdtree')
      d4<-unique(d3$cluster)
      if(length(which(d4==0))>0){
        d5<-length(which(d3$cluster==0))
        d6<-length(d4)-1+d5
      }else{
        d6<-length(d4)
      }
      x0[1, 3]<-nrow(d1)/length(c1)
    }else{
      x0[1, 3]<-1/length(c1)
    }
  }
#  if(nrow(b2)>1){
#    b3<-matrix(c(b2$Longitude, b2$Latitude), ncol=2, byrow=F)
#    c<-dbscan(b3, eps=0.005, minPts = 2, borderPoints = T, search='kdtree')
#    c1<-unique(c$cluster)
#    x0[1, 3]<-nrow(b2)/length(c1)/length(unique(b2$Weeks))
#  }else{
#    x0[1, 3]<-nrow(b2)/length(unique(b2$Weeks))
#  }
  x<-rbind(x, x0)
  cat(i, '\n')
}

colnames(x)<-c('UID', 'WeeklyFreqPlacesVisitTimes', 'WeeklyUnkownPlacesVisitTimes')
write.csv(x, '/Users/yu/Documents/Study/Project/Simulation/Work/20181211/2.Weekly Visit Times.csv', row.names=FALSE)


library(dplyr)
library(ggplot2)
WeeklyVisitTimes<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/2.Weekly Visit Times.csv', stringsAsFactors = FALSE, header=T)
#WeeklyVisitTimes<-x
WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes<-ifelse(WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes>5, WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes-5, WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes)
WeeklyVisitTimes<-rbind(WeeklyVisitTimes, WeeklyVisitTimes)
WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes[(nrow(WeeklyVisitTimes)/2+1):nrow(WeeklyVisitTimes)]<-WeeklyVisitTimes$WeeklyUnkownPlacesVisitTimes[1:(nrow(WeeklyVisitTimes)/2)]
WeeklyVisitTimes$WeeklyUnkownPlacesVisitTimes<-NULL
colnames(WeeklyVisitTimes)<-c('UID', 'WeeklyVisitTimes')
WeeklyVisitTimes$Freq<-'Unknown'
WeeklyVisitTimes$Freq[1:(nrow(WeeklyVisitTimes)/2)]<-'Frequent'
ggplot(WeeklyVisitTimes, aes(x=WeeklyVisitTimes, color=Freq, fill=Freq))+
  geom_histogram(position='identity', alpha=0.5, binwidth=0.5)+
  xlab('Weekly Visit Times')+
  ylab('Number of People')

mean(WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes, na.rm = T) # 2.107861
sd(WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes, na.rm=T) # 0.9264189
fivenum(WeeklyVisitTimes$WeeklyFreqPlacesVisitTimes, na.rm=T) # 1.85183509 

mean(WeeklyVisitTimes$WeeklyUnkownPlacesVisitTimes, na.rm=T) # 1.535932
sd(WeeklyVisitTimes$WeeklyUnkownPlacesVisitTimes, na.rm=T) # 0.3694735
fivenum(WeeklyVisitTimes$WeeklyUnkownPlacesVisitTimes, na.rm = T) # 1.521739

WeeklyVisitTimes<-WeeklyVisitTimes[-which(is.infinite(WeeklyVisitTimes$WeeklyUnkownPlacesVisitTimes)),]
