library(dplyr)
Trip<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/5.Simulation.csv', stringsAsFactors = FALSE, header=T)
colnames(Trip)<-c('UID', 'Place', 'Time', 'SimulationID')
Trip$ActivityType<-NA
Type<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/6.Activity Type.csv', stringsAsFactors = FALSE, header=T)

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:nrow(Type)){
  a<-filter(Trip, Place==Type$Place_Name[i])
  if(nrow(a)>0){
    a$ActivityType<-Type$ActivityType[i]
    x<-rbind(x, a)
  }
  cat(i, '\n')
}

Trip$ActivityType[which(Trip$Place=='Home')]<-'Home'
Trip$ActivityType[which(Trip$Place=='Work')]<-'Work'
a<-filter(Trip, ActivityType=='Home')
x<-rbind(x, a)
a<-filter(Trip, ActivityType=='Work')
x<-rbind(x, a)
table(x$ActivityType)
