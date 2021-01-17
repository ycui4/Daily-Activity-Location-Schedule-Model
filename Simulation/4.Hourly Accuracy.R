library(dplyr)
x<-data.frame(matrix(0, nrow=24, ncol=4))
x[, 1]<-0:23
colnames(x)<-c('Hour', 'L1', 'L2', 'L3')
##### L1
Validation<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Previous One/Trip/6.0Validation.csv', stringsAsFactors = FALSE, header = T)

for(i in 3:26){
  a<-filter(Validation, floor(ArriveTime)==i)
  x[i-3, 2]<-mean(a$Accuracy, na.rm=T)
}

##### L2
Validation<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Combined Trip/8.1Validation Combined.csv', stringsAsFactors = FALSE, header = T)

for(i in 3:26){
  a<-filter(Validation, floor(Time)==i)
  x[i-3, 3]<-mean(a$Accuracy, na.rm=T)
}

##### L3
Validation<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/9.1 Validation.csv', stringsAsFactors = FALSE, header = T)

for(i in 3:26){
  a<-filter(Validation, floor(Time)==i)
  x[i-3, 4]<-mean(a$Accuracy, na.rm=T)
}

write.csv(x, '/Users/yu/Documents/Study/Project/Simulation/Work/20181211/4.Hourly Accuracy.csv', row.names=FALSE)
