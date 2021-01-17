library(dplyr)
L1<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Which Score/1.Level1.csv', header=T, stringsAsFactors = FALSE)
L2<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Which Score/2.Level2.csv', header=T, stringsAsFactors = FALSE)
L3<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Which Score/3.Level3.csv', header=T, stringsAsFactors = FALSE)


### Frequent Places
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:nrow(L3)){
  a1<-filter(L1, DateNew==L3$DateNew[i] & TimeNew==L3$TimeNew[i])
  a1<-a1[1, ]
  a2<-filter(L2, DateNew==L3$DateNew[i] & TimeNew==L3$TimeNew[i])
  
  if(nrow(a1)+nrow(a2)==2){
    a1$UsedScore2<-a2$UsedScore[1]
    a1$UsedScore3<-L3$UsedScore[i]
    x<-rbind(x, a1)
  }
  
  cat(i, '\n')
}
write.csv(x, '/Users/yu/Desktop/Trip1/a.csv', row.names = FALSE)

a<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Which Score/4.Frequent Places.csv', header=T, stringsAsFactors = FALSE)
a1<-filter(a, !is.na(UsedScore))
plot.new()
for(i in 1:nrow(a1)){
  if(a1$UsedScore[i]==1){
    segments(i/nrow(a1), 2/3, i/nrow(a1), 1, col='black')
  }else if(a1$UsedScore[i]==2){
    segments(i/nrow(a1), 2/3, i/nrow(a1), 1, col='red')
  }else if(a1$UsedScore[i]==3){
    segments(i/nrow(a1), 2/3, i/nrow(a1), 1, col='blue')
  }
  
  if(a1$UsedScore2[i]==1){
    segments(i/nrow(a1), 1/3, i/nrow(a1), 2/3, col='black')
  }else if(a1$UsedScore2[i]==2){
    segments(i/nrow(a1), 1/3, i/nrow(a1), 2/3, col='red')
  }else if(a1$UsedScore2[i]==3){
    segments(i/nrow(a1), 1/3, i/nrow(a1), 2/3, col='blue')
  }
  
  if(a1$UsedScore3[i]==1){
    segments(i/nrow(a1), 0, i/nrow(a1), 1/3, col='black')
  }else if(a1$UsedScore3[i]==2){
    segments(i/nrow(a1), 0, i/nrow(a1), 1/3, col='red')
  }else if(a1$UsedScore3[i]==3){
    segments(i/nrow(a1), 0, i/nrow(a1), 1/3, col='blue')
  }
  cat(i, '\n')
}

### Unknon Places
x<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:nrow(L2)){
  if(L2$CurPlace[i]=='Unknow'){
    a1<-filter(L3, DateNew==L2$DateNew[i] & TimeNew==L2$TimeNew[i])
    a2<-L2[i, ]
    if(nrow(a1)>0){
      a1<-a1[1,]
      a2$UsedScore3<-a1$UsedScore
      x<-rbind(x, a2)
    }
  }
  
  cat(i, '\n')
}
write.csv(x, '/Users/yu/Desktop/Trip1/b.csv', row.names = FALSE)

a<-read.csv('/Users/yu/Documents/Study/Project/Simulation/Work/20190124 Survival Model/Which Score/4.Unknown Places.csv', header=T, stringsAsFactors = FALSE)
a1<-filter(a, PredActivityType=='Unknow')

plot.new()
for(i in 1:nrow(a1)){
  if(a1$UsedScore[i]==1){
    segments(i/nrow(a1), 1/3, i/nrow(a1), 2/3, col='black')
  }else if(a1$UsedScore[i]==2){
    segments(i/nrow(a1), 1/3, i/nrow(a1), 2/3, col='red')
  }else if(a1$UsedScore[i]==3){
    segments(i/nrow(a1), 1/3, i/nrow(a1), 2/3, col='blue')
  }
  
  if(a1$UsedScore3[i]==1){
    segments(i/nrow(a1), 0, i/nrow(a1), 1/3, col='black')
  }else if(a1$UsedScore3[i]==2){
    segments(i/nrow(a1), 0, i/nrow(a1), 1/3, col='red')
  }else if(a1$UsedScore3[i]==3){
    segments(i/nrow(a1), 0, i/nrow(a1), 1/3, col='blue')
  }
  cat(i, '\n')
}

#### L1 - Activity Type
## Home, Work, School, Personal, Recreation, Shopping, Trans, Other
plot.new()
for(i in 1:nrow(L1)){
  col<-switch(L1$UsedScore[i], 'black', 'red','blue')
  if(L1$PredActivityType[i]=='Home'){
    segments(i/nrow(L1), 7/8, i/nrow(L1), 1, col=col)
  }else if(L1$PredActivityType[i]=='Work'){
    segments(i/nrow(L1), 6/8, i/nrow(L1), 7/8, col=col)
  }else if(L1$PredActivityType[i]=='School'){
    segments(i/nrow(L1), 5/8, i/nrow(L1), 6/8, col=col)
  }else if(L1$PredActivityType[i]=='Personal'){
    segments(i/nrow(L1), 4/8, i/nrow(L1), 5/8, col=col)
  }else if(L1$PredActivityType[i]=='Recreation'){
    segments(i/nrow(L1), 3/8, i/nrow(L1), 4/8, col=col)
  }else if(L1$PredActivityType[i]=='Shopping'){
    segments(i/nrow(L1), 2/8, i/nrow(L1), 3/8, col=col)
  }else if(L1$PredActivityType[i]=='Trans'){
    segments(i/nrow(L1), 1/8, i/nrow(L1), 2/8, col=col)
  }else if(L1$PredActivityType[i]=='Other'){
    segments(i/nrow(L1), 0, i/nrow(L1), 1/8, col=col)
  }
  
  cat(i, '\n')
}


#### L2 - Activity Type
## Home, Work, School, Personal, Recreation, Shopping, Trans, Other
plot.new()
for(i in 1:nrow(L2)){
  col<-switch(L2$UsedScore[i], 'black', 'red','blue')
  if(L2$PredActivityType[i]=='Home'){
    segments(i/nrow(L2), 7/8, i/nrow(L2), 1, col=col)
  }else if(L2$PredActivityType[i]=='Work'){
    segments(i/nrow(L2), 6/8, i/nrow(L2), 7/8, col=col)
  }else if(L2$PredActivityType[i]=='School'){
    segments(i/nrow(L2), 5/8, i/nrow(L2), 6/8, col=col)
  }else if(L2$PredActivityType[i]=='Personal'){
    segments(i/nrow(L2), 4/8, i/nrow(L2), 5/8, col=col)
  }else if(L2$PredActivityType[i]=='Recreation'){
    segments(i/nrow(L2), 3/8, i/nrow(L2), 4/8, col=col)
  }else if(L2$PredActivityType[i]=='Shopping'){
    segments(i/nrow(L2), 2/8, i/nrow(L2), 3/8, col=col)
  }else if(L2$PredActivityType[i]=='Trans'){
    segments(i/nrow(L2), 1/8, i/nrow(L2), 2/8, col=col)
  }else if(L2$PredActivityType[i]=='Other'){
    segments(i/nrow(L2), 0, i/nrow(L2), 1/8, col=col)
  }
  
  cat(i, '\n')
}


#### L3 - Activity Type
## Home, Work, School, Personal, Recreation, Shopping, Trans, Other
plot.new()
for(i in 1:nrow(L3)){
  col<-switch(L3$UsedScore[i], 'black', 'red','blue')
  if(L3$PredActivityType[i]=='Home'){
    segments(i/nrow(L3), 7/8, i/nrow(L3), 1, col=col)
  }else if(L3$PredActivityType[i]=='Work'){
    segments(i/nrow(L3), 6/8, i/nrow(L3), 7/8, col=col)
  }else if(L3$PredActivityType[i]=='School'){
    segments(i/nrow(L3), 5/8, i/nrow(L3), 6/8, col=col)
  }else if(L3$PredActivityType[i]=='Personal'){
    segments(i/nrow(L3), 4/8, i/nrow(L3), 5/8, col=col)
  }else if(L3$PredActivityType[i]=='Recreation'){
    segments(i/nrow(L3), 3/8, i/nrow(L3), 4/8, col=col)
  }else if(L3$PredActivityType[i]=='Shopping'){
    segments(i/nrow(L3), 2/8, i/nrow(L3), 3/8, col=col)
  }else if(L3$PredActivityType[i]=='Trans'){
    segments(i/nrow(L3), 1/8, i/nrow(L3), 2/8, col=col)
  }else if(L3$PredActivityType[i]=='Other'){
    segments(i/nrow(L3), 0, i/nrow(L3), 1/8, col=col)
  }
  
  cat(i, '\n')
}

#### ALL
plot.new()
for(i in 1:nrow(L1)){
  col<-switch(L1$UsedScore[i], 'black', 'red', 'blue')
  segments(i/nrow(L1), 2/3, i/nrow(L1), 1, col=col)
  cat(i, '\n')
}
for(i in 1:nrow(L2)){
  col<-switch(L2$UsedScore[i], 'black', 'red', 'blue')
  segments(i/nrow(L2), 1/3, i/nrow(L2), 2/3, col=col)
  cat(i, '\n')
}
for(i in 1:nrow(L3)){
  col<-switch(L3$UsedScore[i], 'black', 'red', 'blue')
  segments(i/nrow(L3), 0, i/nrow(L3), 1/3, col=col)
  cat(i, '\n')
}












