library(dplyr)
for(n in 13:15){
  GVS<-read.csv(paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/Trip/7.GVS', n, '.csv'), stringsAsFactors = FALSE, header=T)
  TVS<-read.csv(paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/Trip/7.TVS', n, '.csv'), stringsAsFactors = FALSE, header=T)
  TVS$HourID<-ifelse(TVS$Hour<3, TVS$Hour+24, TVS$Hour)
  PVS<-read.csv(paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/Trip/7.PVS', n, '.csv'), stringsAsFactors = FALSE, header=T)
  PVS$HourID<-ifelse(PVS$Hour<3, PVS$Hour+24, PVS$Hour)
  Go<-read.csv(paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/Trip/4.Go', n, '.csv'), stringsAsFactors = FALSE, header=T)
  StayDuration<-read.csv(paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181201/Unknow DBSCAN/Trip/3.Training', n, '.csv'), stringsAsFactors = FALSE, header=T)
  StayDuration$Hour<-floor(StayDuration$Time)
  StayDuration$HourID<-floor(StayDuration$TimeNew)
  UID0<-unique(GVS$UID)
  
  #### Determine Duration ###
  f1<-function(x){
    if(nrow(x)>0){
      x<-filter(x, StayDuration<4)
      if(nrow(x)>1){
        c1<-x$StayDuration
        c2<-floor(runif(1, min=1, max=length(c1)))
        c3<-c1[c2]
        return(c3)
      }else if(nrow(x)==1){
        return(x$StayDuration[1])
      }else{
        return(15/60)
      }
    }else{
      return(15/60)
    }
  }
  
  x<-data.frame(matrix(0, nrow=0, ncol=2))
  for(i in 1:length(UID0)){
    a1<-filter(GVS, UID==UID0[i])
    a2<-filter(TVS, UID==UID0[i])
    a3<-filter(PVS, UID==UID0[i])
    a4<-filter(Go, UID==UID0[i])
    a5<-filter(StayDuration, UID==UID0[i])
    
    SimulationID<-1
    
    while(SimulationID<=300){
      x1<-data.frame(matrix(c(UID0[i], 'Home', 3, SimulationID), nrow=1, byrow=T), stringsAsFactors = FALSE)
      Time<-3
      Time0<-3
      PreviousPlace<-'Home'
      b1<-filter(a5, HourID==3 & CurPlace=='Home')
      p<-c()
      if(nrow(b1)>0){
        b1<-filter(b1, StayDuration<4)
        if(nrow(b1)>1){
          c1<-density(b1$StayDuration)
          c2<-which.max(c1$y)
          c3<-c1$x[c2]
          Time<-Time+c3
          Time0<-Time0+c3
        }else if(nrow(b1)==1){
          s<-b1$StayDuration[1]
          Time<-Time+s
          Time0<-Time0+s
        }else{
          Time<-6
          Time0<-6
        }
      }else{
        Time<-6
        Time0<-6
      }
      
      while(Time<27){
        Time0<-ifelse(Time<24, Time, Time-24)
        x0<-data.frame(matrix(0, nrow=1, ncol=4)) ### UID, Place, Time, SimulationID
        
        #b1<-filter(a1, PrePlace==PreviousPlace)
        #b2<-filter(a2, PrePlace==PreviousPlace)
        #b3<-filter(a3, PrePlace==PreviousPlace)
        b4<-filter(a4, Hour==floor(Time0) & PrePlace==PreviousPlace)
        #b5<-filter(a5, PrePlace==PreviousPlace)
        
        if(nrow(b4)==0){
          x0[1, 1]<-UID0[i]
          x0[1, 2]<-PreviousPlace
          x0[1, 3]<-Time
          x0[1, 4]<-SimulationID
          Time<-Time+15/60
          Time0<-Time0+15/60
          PreviousPlace<- PreviousPlace
        }else if(nrow(b4)==1){
          c1<-setdiff(b4$CurPlace[1], p)
          if(length(c1)>0){
            x0[1, 1]<-UID0[i]
            x0[1 ,2]<-b4$CurPlace[1]
            x0[1, 3]<-Time
            x0[1, 4]<-SimulationID
            b5<-filter(a5, Hour==floor(Time0) & PrePlace==PreviousPlace & CurPlace==b4$CurPlace[1])
            s<-f1(b5)
            Time<-Time+s
            Time0<-Time0+s
            PreviousPlace<-b4$CurPlace[1]
          }else{
            x0[1, 1]<-UID0[i]
            x0[1, 2]<-PreviousPlace
            x0[1, 3]<-Time
            x0[1, 4]<-SimulationID
            Time<-Time+15/60
            Time0<-Time0+15/60
            PreviousPlace<- PreviousPlace
          }
          
        }else{
          c1<-unique(b4$CurPlace)
          c2<-setdiff(c1, p)
          
          if(length(c2)>0){
            Score<-data.frame(matrix(0, nrow=length(c2), ncol=4)) ### Place, GUF, TUF, PUF
            colnames(Score)<-c('Place', 'GVS', 'TVS', 'PVS')
            Score$Place<-c2
            for(j in 1:nrow(Score)){
              Score$GVS[j]<-a1$GFS[which(a1$PrePlace==PreviousPlace & a1$CurPlace==Score$Place[j])]
              Score$TVS[j]<-a2$TFS[which(a2$Hour==floor(Time0) & a2$PrePlace==PreviousPlace & a2$CurPlace==Score$Place[j])]
              tryCatch({
                Score$PVS[j]<-a3$PVS[which(a3$Hour==floor(Time0) & a3$Place==Score$Place[j])]
              }, error=function(e){
                Score$PVS[j]<-0
              })
              
            }
            if(sum(Score$GVS)>0){
              Score$GVS<-Score$GVS/sum(Score$GVS)
              e1<--sum(Score$GVS * log(Score$GVS), na.rm=T)
            }else{
              e1<-1000000
            }
            if(sum(Score$TVS)>0){
              Score$TVS<-Score$TVS/sum(Score$TVS)
              e2<--sum(Score$TVS * log(Score$TVS), na.rm=T)
            }else{
              e2<-1000000
            }
            if(sum(Score$PVS)>0){
              Score$PVS<-Score$PVS/sum(Score$PVS)
              e3<--sum(Score$PVS * log(Score$PVS), na.rm=T)
            }else{
              e3<-1000000
            }
            
            e0<-which.min(c(e1, e2, e3))
            e4<-ceiling(Score[, e0+1]*1000)
            g0<-c()
            for(j in 1:length(e4)){
              g0<-c(g0, rep(Score$Place[j], e4[j]))
            }
            g1<-sample(g0)
            g2<-floor(runif(1, min=1, max=length(g1)))
            
            x0[1, 1]<-UID0[i]
            x0[1, 2]<-g1[g2]
            x0[1, 3]<-Time
            x0[1, 4]<-SimulationID
            
            b5<-filter(a5, Hour==floor(Time0) & PrePlace==PreviousPlace & CurPlace==g1[g2])
            s<-f1(b5)
            Time<-Time+s
            Time0<-Time0+s
            PreviousPlace<-g1[g2]
          }else{
            x0[1, 1]<-UID0[i]
            x0[1, 2]<-PreviousPlace
            x0[1, 3]<-Time
            x0[1, 4]<-SimulationID
            Time<-Time+15/60
            Time0<-Time0+15/60
            PreviousPlace<- PreviousPlace
          }
        }
        x1<-rbind(x1, x0)
        if(!x0[1, 2]%in%c('Home', 'Work')){
          p<-c(p, x0[1, 2])
          p<-unique(p)
        }
      }
      
      #### Combine Trips
      for(j in 1:(nrow(x1)-1)){
        if(x1$X2[j]==x1$X2[j+1]){
          x1$X3[j+1]<-x1$X3[j]
        }
      }
      x2<-unique(x1)
      x<-rbind(x, x2)
      SimulationID<-SimulationID+1
    }
      
    cat(i, '\n')
  }
  
  colnames(x)<-c('UID', 'Place', 'Time')
  write.csv(x, paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/Simulation//1.Simulation', n, '.csv'), row.names=FALSE)
  cat(n, '\n')
}


x<-data.frame(matrix(0, nrow=0, ncol=2))
for(n in 1:15){
  a<-read.csv(paste0('/Users/yu/Documents/Study/Project/Simulation/Work/20181211/Simulation//1.Simulation', n, '.csv'), stringsAsFactors = FALSE, header=T)
  x<-rbind(x, a)
  cat(n, '\n')
}
write.csv(x, '/Users/yu/Documents/Study/Project/Simulation/Work/20181211/Simulation//1.Simulation.csv', row.names=FALSE)


