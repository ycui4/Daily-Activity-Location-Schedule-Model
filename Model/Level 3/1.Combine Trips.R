library(dplyr)
Data0<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/3.Validation.csv', header=T, stringsAsFactors = FALSE)

#Data0$PrePlace[which(Data0$NewTripID==1)]<-NA

UID0<-unique(Data0$UID)
UID0<-UID0[1201:length(UID0)]

x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:length(UID0)){
  a1<-filter(Data0, UID==UID0[i])
  a2<-unique(a1$DateNew)
  
  for(j in 1:length(a2)){
    b1<-filter(a1, DateNew==a2[j])
    b1<-b1[order(b1$TimeNew), ]
    
    if(nrow(b1)>1){
      for(k in 2:nrow(b1)){
        if(b1$CurPlace[k]==b1$CurPlace[k-1]){
          #c1<-sum(b1$StayDuration[k], b1$StayDuration[k+1])
          c1<-b1$StayDuration[k]
          b1[k,]<-b1[k-1,]
          b1$StayDuration[k]<-c1
        }
      }
      b1[is.na(b1)] <- 0
      b2<-aggregate(StayDuration~., data=b1, FUN = sum)
      b2<-b2[order(b2$TimeNew),]
      #b2[which(b2==0)]<-NA
    }else{
      b2<-b1
    }
    
    x<-rbind(x, b2)
  }
  cat(i, '\n')
}

x$PrePlace[which(x$PrePlace==0)]<-NA

write.csv(x, '/Users/yu/Desktop/Trip1/a5.csv', row.names = FALSE)

a1<-read.csv('/Users/yu/Desktop/Trip1/a1.csv', header=T, stringsAsFactors = FALSE)
a2<-read.csv('/Users/yu/Desktop/Trip1/a2.csv', header=T, stringsAsFactors = FALSE)
a3<-read.csv('/Users/yu/Desktop/Trip1/a3.csv', header=T, stringsAsFactors = FALSE)
a4<-read.csv('/Users/yu/Desktop/Trip1/a4.csv', header=T, stringsAsFactors = FALSE)
a5<-read.csv('/Users/yu/Desktop/Trip1/a5.csv', header=T, stringsAsFactors = FALSE)
a<-rbind(a1, a2, a3, a4, a5)
write.csv(a, '/Users/yu/Desktop/Trip1/a.csv', row.names = FALSE)
