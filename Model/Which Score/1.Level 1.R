### load 3.Survial model and run until res.cox
library(dplyr)
DataV0<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 1/2.Validation.csv', header=T, stringsAsFactors = FALSE)
colnames(DataV0)<-c("UID", "PrePlace", "CurPlace", "Month", "DateNew", "TimeNew", "StayDuration", "HHSize", "Gender", "Age", "Race", "ActivityType", "PreHome", "PreWork", "PreSchool", "PrePersonal", "PreRecreation", "PreShopping", "PreTrans", "PreOther")

GUS<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Previous One/4.GUS.csv', stringsAsFactors = FALSE, header=T)
TUS<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Previous One/4.TUS.csv', stringsAsFactors = FALSE, header=T)
GUF<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/3 Week Model 1 Week Validate/4.GUF.csv', stringsAsFactors = FALSE, header=T)
TUF<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/3 Week Model 1 Week Validate/4.TUF.csv', stringsAsFactors = FALSE, header=T)
PUF<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/3 Week Model 1 Week Validate/4.PUF.csv', stringsAsFactors = FALSE, header=T)
ActivityType<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/6.Activity Type.csv', stringsAsFactors = FALSE, header=T)

testID<-47338:50000
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(testID)){
  a0<-DataV0[testID[i], ]
  if(is.na(a0$PrePlace)){
    a1<-filter(GUF, UID==a0$UID)
    a2<-filter(TUF, UID==a0$UID & Hour==floor(a0$TimeNew))
    a3<-filter(PUF, UID==a0$UID & Hour==floor(a0$TimeNew))
    
    b1<-unique(a3$Place)
    
    Score<-data.frame(matrix(0, nrow=length(b1), ncol=4)) # Place, GUF, TUF, PUF
    colnames(Score)<-c('Place', 'GUS', 'TUS', 'PUS')
    Score$Place<-b1
    for(j in 1:nrow(Score)){
      tryCatch({
        Score$GUS[j]<-a1$GUF[which(a1$Place==Score$Place[j])]
      }, error=function(e){
        Score$GUS[j]<-0
      })
      
      tryCatch({
        Score$TUS[j]<-a2$TUF[which(a2$Place==Score$Place[j])]
      }, error=function(e){
        Score$TUS[j]<-0
      })
      
      Score$PUS[j]<-a3$PUF[which(a3$Place==Score$Place[j])]
    }
    
  }else{
    a1<-filter(GUS, UID==a0$UID & PrePlace==a0$PrePlace)
    a2<-filter(TUS, UID==a0$UID & PrePlace==a0$PrePlace & Hour==floor(a0$TimeNew))
    a3<-filter(PUF, UID==a0$UID & Hour==floor(a0$TimeNew))
    
    b1<-unique(a3$Place)
    
    Score<-data.frame(matrix(0, nrow=length(b1), ncol=4)) # Place, GUF, TUF, PUF
    colnames(Score)<-c('Place', 'GUS', 'TUS', 'PUS')
    Score$Place<-b1
    for(j in 1:nrow(Score)){
      tryCatch({
        Score$GUS[j]<-a1$GFS[which(a1$CurPlace==Score$Place[j])]
      }, error=function(e){
        Score$GUS[j]<-0
      })
      
      tryCatch({
        Score$TUS[j]<-a2$TFS[which(a2$CurPlace==Score$Place[j])]
      }, error=function(e){
        Score$TUS[j]<-0
      })
      
      Score$PUS[j]<-a3$PUF[which(a3$Place==Score$Place[j])]
    }
    
  }
  
  if(nrow(Score)>0){
    if(sum(Score$GUS)>0){
      Score$GUS<-Score$GUS/sum(Score$GUS)
      c1<--sum(Score$GUS*log(Score$GUS), na.rm=T)
    }else{
      c1<-1000000
    }
    
    if(sum(Score$TUS)>0){
      Score$TUS<-Score$TUS/sum(Score$TUS)
      c2<--sum(Score$TUS*log(Score$TUS), na.rm=T)
    }else{
      c2<-1000000
    }
    
    if(sum(Score$PUS)>0){
      Score$PUS<-Score$PUS/sum(Score$PUS)
      c3<--sum(Score$PUS*log(Score$PUS), na.rm=T)
    }else{
      c3<-1000000
    }
    
    d1<-which.min(c(c1, c2, c3))
    d2<-which.max(Score[, (d1+1)])
    d3<-Score$Place[d2]
    if(d3=='Home'){
      d4<-'Home'
    }else if(d3=='Work'){
      d4<-'Work'
    }else{
      d4<-ActivityType$ActivityType[which(ActivityType$Place_Name==d3)]
    }
    
    a0$PredPlace<-d3
    a0$PredActivityType<-d4
    a0$UsedScore<-d1
    
    x<-rbind(x, a0)
  }
  
  cat(i, '\n')
}

write.csv(x, '/Users/yu/Desktop/Trip1/a1.5.csv', row.names=FALSE)

a1<-read.csv('/Users/yu/Desktop/Trip1/a1.csv', header=T, stringsAsFactors = FALSE)
a2<-read.csv('/Users/yu/Desktop/Trip1/a1.1.csv', header=T, stringsAsFactors = FALSE)
a3<-read.csv('/Users/yu/Desktop/Trip1/a1.2.csv', header=T, stringsAsFactors = FALSE)
a4<-read.csv('/Users/yu/Desktop/Trip1/a1.3.csv', header=T, stringsAsFactors = FALSE)
a5<-read.csv('/Users/yu/Desktop/Trip1/a1.4.csv', header=T, stringsAsFactors = FALSE)
a6<-read.csv('/Users/yu/Desktop/Trip1/a1.5.csv', header=T, stringsAsFactors = FALSE)
a7<-read.csv('/Users/yu/Desktop/Trip1/a2.csv', header=T, stringsAsFactors = FALSE)
a8<-read.csv('/Users/yu/Desktop/Trip1/a2.2.csv', header=T, stringsAsFactors = FALSE)
a9<-read.csv('/Users/yu/Desktop/Trip1/a3.csv', header=T, stringsAsFactors = FALSE)
a10<-read.csv('/Users/yu/Desktop/Trip1/a3.1.csv', header=T, stringsAsFactors = FALSE)
a11<-read.csv('/Users/yu/Desktop/Trip1/a4.csv', header=T, stringsAsFactors = FALSE)
a12<-read.csv('/Users/yu/Desktop/Trip1/a5.csv', header=T, stringsAsFactors = FALSE)
a<-rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) 
write.csv(a, '/Users/yu/Desktop/Trip1/a.csv', row.names = FALSE)

table(a$UsedScore)   ### 1-24551, 2-90923, 3-80194








