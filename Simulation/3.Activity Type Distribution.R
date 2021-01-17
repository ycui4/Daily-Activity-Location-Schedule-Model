library(dplyr)
Trip<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/1.Trip.csv', stringsAsFactors = FALSE, header=T)
Trip$ActivityType<-NA
Type<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/6.Activity Type.csv', stringsAsFactors = FALSE, header=T)

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:nrow(Type)){
  a<-filter(Trip, CurPlace==Type$Place_Name[i])
  if(nrow(a)>0){
    a$ActivityType<-Type$ActivityType[i]
    x<-rbind(x, a)
  }
  cat(i, '\n')
}
Trip$ActivityType[which(Trip$CurPlace=='Home')]<-'Home'
Trip$ActivityType[which(Trip$CurPlace=='Work')]<-'Work'
a<-filter(Trip, ActivityType%in%c('Home', 'Work'))
x<-rbind(x, a)

Activity<-x
table(x$ActivityType)
Activity$Weekday<-weekdays(as.Date(as.character(Activity$DateNew), '%Y%m%d'))
ActivityWeekday<-filter(Activity, !Weekday%in%c('Saturday', 'Sunday'))
table(ActivityWeekday$ActivityType)


Survey<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Data/SurveyData/1.OnlineRegistration-RaceEthnicityAge.csv', stringsAsFactors = FALSE, header=T)
UID0<-unique(ActivityWeekday$UID)
ActivityWeekday$Gender<-NA
ActivityWeekday$Age<-NA
ActivityWeekday$Race<-NA

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  a1<-filter(ActivityWeekday, UID==UID0[i])
  a2<-filter(Survey, Email==UID0[i])
  
  if(nrow(a2)>0){
    a1$Gender<-a2$gender
    a1$Age<-a2$What.is.your.age..x
    a1$Race<-a2$What.is.your.race..x
  }
  x<-rbind(x, a1)
  cat(i, '\n')
}

ActivityWeekday<-filter(ActivityWeekday, !is.na(Age))
UID0<-unique(ActivityWeekday$UID)

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  a<-filter(Survey, Email==UID0[i])
  if(nrow(a)>0){
    x0<-data.frame(matrix(0, nrow=1, ncol=4))
    x0[1, 1]<-UID0[i]
    x0[1, 2]<-a$gender
    x0[1, 3]<-a$What.is.your.age..x
    x0[1, 4]<-a$What.is.your.race..x
    x<-rbind(x, x0)
  }
  cat(i, '\n')
}
colnames(x)<-c('UID', 'Gender', 'Age', 'Race')
Demo<-x
table(Demo$Gender)
table(Demo$Age)
table(Demo$Race)

ActivityWeekday<-x
ActivityWeekday<-filter(ActivityWeekday, Gender!="" & Age!="" & Race!="")
table(ActivityWeekday$Gender, ActivityWeekday$ActivityType)
table(ActivityWeekday$Age, ActivityWeekday$ActivityType)
table(ActivityWeekday$Race, ActivityWeekday$ActivityType)

table(ActivityWeekday$Race)
Demo$Race[10]


mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Home')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Work')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='School')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Shopping')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Personal')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Recreation')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Trans')], na.rm=T)
mean(ActivityWeekday$StayDuration[which(ActivityWeekday$ActivityType=='Other')], na.rm=T)

write.csv(ActivityWeekday, '/Users/yu/Desktop/1/1.Survey.csv', row.names = FALSE)

