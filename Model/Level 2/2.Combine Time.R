library(dplyr)
#library(survival)
#library(survminer)

Data0<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 2/1.Validation.csv', header=T, stringsAsFactors = FALSE)
Data<-data.frame(Data0$UID, Data0$PrePlace, Data0$CurPlace, Data0$Month, Data0$DateNew,  Data0$TimeNew, Data0$StayDuration, stringsAsFactors = FALSE)
Data$HHSize<-NA
Data$Gender<-NA
Data$Age<-NA
Data$Race<-NA
Data$ActivityType<-NA

Survey<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Data/SurveyData/1.OnlineRegistration-RaceEthnicityAge.csv', header=T, stringsAsFactors = FALSE)

UID0<-unique(Data$Data0.UID)

for(i in 1:length(UID0)){
  a1<-filter(Survey, Email==UID0[i])
  Data$HHSize[which(Data$Data0.UID==UID0[i])]<-a1$Household.Size.Reported
  Data$Gender[which(Data$Data0.UID==UID0[i])]<-a1$gender
  Data$Age[which(Data$Data0.UID==UID0[i])]<-a1$age
  tryCatch({
    Data$Race[which(Data$Data0.UID==UID0[i])]<-a1$What.is.your.race..x
  }, error=function(e){
    Data$RaceData$Race[which(Data$Data0.UID==UID0[i])]<-NA
  })
  
  
  cat(i, '\n')
}


ActivityType<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/6.Activity Type.csv', header=T, stringsAsFactors = FALSE)

#place0<-unique(ActivityType$Place_Name)

for(i in 1:nrow(ActivityType)){
  Data$ActivityType[which(Data$Data0.CurPlace==ActivityType$Place_Name[i])]<-ActivityType$ActivityType[i]
  cat(i, '\n')
}

Data$ActivityType<-ifelse(Data$Data0.CurPlace=='Home', 'Home', Data$ActivityType)
Data$ActivityType<-ifelse(Data$Data0.CurPlace=='Work', 'Work', Data$ActivityType)
Data$ActivityType<-ifelse(Data$Data0.CurPlace=='Unknow', 'Unknow', Data$ActivityType)

Data0<-filter(Data, !is.na(ActivityType))

Data0$PreHome<-0
Data0$PreWork<-0
Data0$PreSchool<-0
Data0$PrePersonal<-0
Data0$PreRecreation<-0
Data0$PreShopping<-0
Data0$PreTrans<-0
Data0$PreOther<-0
Data0$PreUnknow<-0

UID0<-UID0[1:500]

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  a1<-filter(Data0, Data0.UID==UID0[i])
  a2<-unique(a1$Data0.DateNew)
  
  for(j in 1:length(a2)){
    x0<-data.frame(matrix(0, nrow=0, ncol=2))
    b1<-filter(a1, Data0.DateNew==a2[j])
    b2<-unique(b1$Data0.CurPlace)
    
    for(k in 1:length(b2)){
      c1<-filter(b1, Data0.CurPlace==b2[k])
      c2<-b1$ActivityType[which(b1$Data0.TimeNew<min(c1$Data0.TimeNew))]
      if('Home'%in%c2){
        c1$PreHome<-1
      }
      if('Work'%in%c2){
        c1$PreWork<-1
      }
      if('School'%in%c2){
        c1$PreSchool<-1
      }
      if('Personal'%in%c2){
        c1$PrePersonal<-1
      }
      if('Recreation'%in%c2){
        c1$PreRecreation<-1
      }
      if('Shopping'%in%c2){
        c1$PreShopping<-1
      }
      if('Trans'%in%c2){
        c1$PreTrans<-1
      }
      if('Other'%in%c2){
        c1$PreOther<-1
      }
      if('Unknow'%in%c2){
        c1$PreUnknow<-1
      }
      
      
      if(!b2[k]%in%c('Home', 'Work')){
        c3<-sum(c1$Data0.StayDuration)
        c1$Data0.StayDuration<-c3
        x0<-rbind(x0, c1[which.min(c1$Data0.TimeNew),])
      }else{
        x0<-rbind(x0, c1)
      }
      
    }
    x0<-x0[order(x0$Data0.TimeNew),]
    x<-rbind(x, x0)
  }
  
  cat(i, '\n')
}

write.csv(x, '/Users/yu/Desktop/Trip1/b1.csv', row.names = FALSE)

b1<-read.csv('/Users/yu/Desktop/Trip1/a1.csv', header=T, stringsAsFactors = FALSE)
b2<-read.csv('/Users/yu/Desktop/Trip1/a2.csv', header=T, stringsAsFactors = FALSE)
b3<-read.csv('/Users/yu/Desktop/Trip1/a3.csv', header=T, stringsAsFactors = FALSE)
b<-rbind(b1, b2, b3)
write.csv(b, '/Users/yu/Desktop/Trip1/b.csv', row.names = FALSE)
