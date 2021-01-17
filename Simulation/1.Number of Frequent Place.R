library(dplyr)
Freq<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181101/5.FreqPlacesFreqNY_HW_Coord_Fill.csv', stringsAsFactors = FALSE, header=T)
Trip1<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Combined Trip/3.Training.csv', stringsAsFactors = FALSE, header=T)
Trip2<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Combined Trip/3.Validation.csv', stringsAsFactors = FALSE, header=T)
Trip<-rbind(Trip1, Trip2)
rm(Trip1, Trip2)
gc()
write.csv(Trip,'/Users/yu/Documents/Study/Project/Simulation/Work/20181211/1.Trip.csv', row.names=FALSE)

UID0<-unique(Freq$UID)
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  x0<-data.frame(matrix(0, nrow=1, ncol=3)) # UID, NumberOfFreqPlace, NumberOfUnreportedPlace
  a<-filter(Freq, UID==UID0[i])
  a<-a[-which(a$Name=='Home'), ]
  a<-a[-which(a$Name=='Work'), ]
  b<-filter(Trip, UID==UID0[i])
  b1<-filter(b, !CurPlace%in%a$Name)
  x0[1, 1]<-UID0[i]
  x0[1, 2]<-nrow(a)
  x0[1, 3]<-nrow(b1)
  x<-rbind(x, x0)
  cat(i, '\n')
}
colnames(x)<-c('UID', 'NumOfFreqPlace', 'NumOfUnreportedPlace')
write.csv(x, '/Users/yu/Documents/Study/Project/Simulation/Work/20181211/1.Number of Frequent Places.csv', row.names=FALSE)

#Num<-x
library(dplyr)
library(ggplot2)
Num<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/1.Number of Frequent Places.csv', stringsAsFactors=FALSE, header=T)
ggplot(Num, aes(x=NumOfFreqPlace))+
  geom_histogram(binwidth = 1, color='orangered', fill='salmon')+
  labs(x='Number of Frequent Places')

ggplot(Num, aes(x=NumOfUnreportedPlace/20))+
  geom_histogram(binwidth = 1, color='deepskyblue', fill='aquamarine')+
  labs(x='Number of Unreported Places')


Num<-rbind(Num, Num)
Num$NumOfFreqPlace[(nrow(Num)/2+1):nrow(Num)]<-Num$NumOfUnreportedPlace[1:(nrow(Num)/2)]/20
Num$NumOfUnreportedPlace<-'Frequent'
Num$NumOfUnreportedPlace[(nrow(Num)/2+1):nrow(Num)]<-'Unknown'
colnames(Num)<-c('UID', 'NumOfPlace', 'Freq')
ggplot(Num, aes(x=NumOfPlace, color=Freq, fill=Freq))+
  geom_histogram(position='identity', alpha=0.5, binwidth=10)+
  xlab('Number of Visited Places')+
  ylab('Number of People')


###Num$NumOfFreqPlace
## mean = 20.60960
## sd = 16.95697
## median = 15
##Num$NumOfUnreportedPlace
## mean = 34.34017
## sd = 18.04577
## median = 30.75

