library(dplyr)
library(survival)
library(survminer)

### Train ###
DataT<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 2/2.Training.csv', header=T, stringsAsFactors = FALSE)
colnames(DataT)<-c("UID", "PrePlace", "CurPlace", "Month", "DateNew", "TimeNew", "StayDuration", "HHSize", "Gender", "Age", "Race", "ActivityType", "PreHome", "PreWork", "PreSchool", "PrePersonal", "PreRecreation", "PreShopping", "PreTrans", "PreOther", 'PreUnknow')
DataT$Status<-1

DataT$SurvObj<-with(DataT, Surv(StayDuration, Status==1))
head(DataT)

res.cox<-coxph(SurvObj~Month+TimeNew+HHSize+Gender+Age+Race+ActivityType+PreHome+PreWork+PreSchool+PrePersonal+PreRecreation+PreShopping+PreTrans+PreOther+PreUnknow, data=DataT)

### Validation
DataV0<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 2/2.Validation.csv', header=T, stringsAsFactors = FALSE)
colnames(DataV0)<-c("UID", "PrePlace", "CurPlace", "Month", "DateNew", "TimeNew", "StayDuration", "HHSize", "Gender", "Age", "Race", "ActivityType", "PreHome", "PreWork", "PreSchool", "PrePersonal", "PreRecreation", "PreShopping", "PreTrans", "PreOther", 'PreUnknow')
#Data0$Status<-1
DataV<-DataV0[, c(4, 6, 8:20)]

RDM<-runif(1000, min=1, max=nrow(DataV))

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(RDM)){
  a1<-survfit(res.cox, DataV[RDM[i], ])
  a2<-summary(a1)$table['median']
  
  x0<-DataV0[RDM[i], ]
  x0$PredictedStayDuration<-a2
  
  x<-rbind(x, x0)
  
  cat(i, '\n')
}

write.csv(x, '/Users/yu/Desktop/Trip1/a5.csv', row.names = FALSE)

a1<-read.csv('/Users/yu/Desktop/Trip1/a1.csv', header=T, stringsAsFactors = FALSE)
a2<-read.csv('/Users/yu/Desktop/Trip1/a2.csv', header=T, stringsAsFactors = FALSE)
a3<-read.csv('/Users/yu/Desktop/Trip1/a3.csv', header=T, stringsAsFactors = FALSE)
a4<-read.csv('/Users/yu/Desktop/Trip1/a4.csv', header=T, stringsAsFactors = FALSE)
a<-rbind(a1, a2, a3, a4)
write.csv(a, '/Users/yu/Desktop/Trip1/a.csv', row.names = FALSE)
a<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 2/3.Survival Length.csv', header=T, stringsAsFactors = FALSE)
#a<-a[-which(is.na(a$StayDuration)),]
#a<-a[-which(a$StayDuration==0),]
a0<-mean(abs(a$PredictedStayDuration-a$StayDuration)/a$StayDuration, na.rm=T)
a$SDDiff<-a$PredictedStayDuration-a$StayDuration

b<-data.frame(matrix(weekdays(as.Date(as.character(a$DateNew), '%Y%m%d')), ncol=1, byrow=F))
colnames(b)<-'Weekdays'
a<-cbind(a, b)
a$Weekdays<-as.factor(a$Weekdays)
aWeekday<-filter(a, !Weekdays%in%c('Saturday', 'Sunday'))
aWeekday1<-filter(aWeekday, abs(SDDiff)<6)
#aWeekday1<-aWeekday[-which(!aWeekday$ActivityType%in%c('Home', 'Work', 'Scool')&&a$PredictedStayDuration>4), ]
mean(abs(aWeekday1$SDDiff)/aWeekday1$StayDuration)  ### 1.17753

c<-filter(aWeekday1, ActivityType=='Home')
mean(abs(c$SDDiff)/c$StayDuration)*1.5
