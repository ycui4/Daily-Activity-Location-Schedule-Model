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

write.csv(Trip, '/Users/yu/Desktop/1/Simulation.csv')


library(dplyr)
Trip<-read.csv('/Users/yu/Desktop/1/Simulation.csv', stringsAsFactors = FALSE, header=T)
Trip$Duration<-NA
UID0<-unique(Trip$UID)
UID0<-UID0[1401:1500]
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  a1<-filter(Trip, UID==UID0[i])
  for(j in 1:300){
    a2<-filter(a1, SimulationID==j)
    if(nrow(a2)>1){
      a2$Duration[1:(nrow(a2)-1)]<-a2$Time[2:nrow(a2)]-a2$Time[1:(nrow(a2)-1)]
      a2$Duration[nrow(a2)]<-27-a2$Time[nrow(a2)]
    }
    x<-rbind(x, a2)
    
  }
  cat(i, '\n')
}
write.csv(x, '/Users/yu/Desktop/1/1.Simulation Duration15.csv', row.names = FALSE)

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(n in 1:15){
  a<-read.csv(paste0('/Users/yu/Desktop/1/1.Simulation Duration', n, '.csv'), stringsAsFactors = FALSE, header=T)
  x<-rbind(x, a)
  cat(n, '\n')
}

for(i in 1:nrow(Type)){
  x$ActivityType[which(x$Place==Type$Place_Name[i])]<-Type$ActivityType[i]
  cat(i, '\n')
}

write.csv(x, '/Users/yu/Desktop/1/1.Simulation Duration Activity Type.csv', row.names = FALSE)

x<-read.csv('/Users/yu/Desktop/1/1.Simulation Duration Activity Type.csv', stringsAsFactors = FALSE, header=T)
mean(x$Duration[which(x$ActivityType=='Home')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Work')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='School')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Shopping')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Personal')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Recreation')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Trans')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Other')], na.rm=T)
mean(x$Duration[which(x$ActivityType=='Home')], na.rm=T)



