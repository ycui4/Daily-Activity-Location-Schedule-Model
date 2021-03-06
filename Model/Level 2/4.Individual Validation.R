### load 3.Survial model and run until res.cox
library(dplyr)
DataV0<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 2/2.Validation.csv', header=T, stringsAsFactors = FALSE)
colnames(DataV0)<-c("UID", "PrePlace", "CurPlace", "Month", "DateNew", "TimeNew", "StayDuration", "HHSize", "Gender", "Age", "Race", "ActivityType", "PreHome", "PreWork", "PreSchool", "PrePersonal", "PreRecreation", "PreShopping", "PreTrans", "PreOther", "PreUnknow")

GVS<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Combined Trip/6.GVS.csv', stringsAsFactors = FALSE, header=T)
TVS<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Combined Trip/6.TVS.csv', stringsAsFactors = FALSE, header=T)
#GUF<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/3 Week Model 1 Week Validate/4.GUF.csv', stringsAsFactors = FALSE, header=T)
#TUF<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/3 Week Model 1 Week Validate/4.TUF.csv', stringsAsFactors = FALSE, header=T)
PVS<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Combined Trip/6.PVS.csv', stringsAsFactors = FALSE, header=T)
ActivityType<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181120 Model/6.Activity Type.csv', stringsAsFactors = FALSE, header=T)

testID<-runif(1000, min=1, max=nrow(DataV0))
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(testID)){
  a0<-DataV0[testID[i], ]
  if(is.na(a0$PrePlace)){
    d3<-'Home'
    d4<-'Home'
    
  }else{
    a1<-filter(GVS, UID==a0$UID & PrePlace==a0$PrePlace)
    a2<-filter(TVS, UID==a0$UID & PrePlace==a0$PrePlace & Hour==floor(a0$TimeNew))
    a3<-filter(PVS, UID==a0$UID & Hour==floor(a0$TimeNew))
    
    b1<-unique(a1$CurPlace)
    
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
      
      tryCatch({
        Score$PUS[j]<-a3$PUF[which(a3$Place==Score$Place[j])]
      }, error=function(e){
        Score$PUS[j]<-0
      })
      
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
      }else if(d3=='Unknow'){
        d4<-'Unknow'
      }else{
        d4<-ActivityType$ActivityType[which(ActivityType$Place_Name==d3)]
      }
    
  }
    e1<-a0[, c(4, 6, 8:21)]
    e2<-survfit(res.cox, e1)
    e3<-summary(e2)$table['median']
    
    a0$PredPlace<-d3
    a0$PredActivityType<-d4
    a0$PredStayDuration1<-e3  ### use true Activity Type
    
    if(d4!=e1$ActivityType){
      e4<-e1
      e4$ActivityType<-d4
      e5<-survfit(res.cox, e4)
      e6<-summary(e5)$table['median']
      
      #a0$PredStayDuration1<-e3  ### use true Activity Type
      a0$PredStayDuration2<-e6  ### use predicted Activity Type
    }else{
      #a0$PredStayDuration1<-e3  ### use true Activity Type
      a0$PredStayDuration2<-e3  ### use predicted Activity Type
    }
    
    x<-rbind(x, a0)
  }
  
  cat(i, '\n')
}

write.csv(x, '/Users/yu/Desktop/Trip1/a46.csv', row.names=FALSE)

a<-data.frame(matrix(0, nrow=0, ncol=2))
 for(i in 1:46){
   a1<-read.csv(paste0('/Users/yu/Desktop/Trip1/a', i, '.csv'), header=T, stringsAsFactors = FALSE)
   a<-rbind(a, a1)
 }

write.csv(a, '/Users/yu/Desktop/Trip1/a.csv', row.names = FALSE)
a<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Level 2/4.Individual Validation.csv', header=T, stringsAsFactors = FALSE)

mean(abs(a$StayDuration-a$PredStayDuration1)/a$StayDuration)

length(which(a$CurPlace==a$PredPlace))/nrow(a)






