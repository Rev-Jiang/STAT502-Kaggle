set.seed(1234)
dat<-read.csv("/Users/Anesthesia/Desktop/Spring 2020/STAT502/class project/isu-stat-502-sp-2020/train.csv")
house<-dat
house$property<-NULL
house_process<-house
#house_process$date<-as.factor(house$date)
house_process$date<-NULL
house_process$lat<-NULL
house_process$long<-NULL
house_process$waterfront<-as.factor(house$waterfront)
house_process$zipcode<-as.factor(house$zipcode)
house_process$view<-as.factor(house$view)
for(i in 1:10000){
  if(house_process$yr_built[i]>house_process$yr_renovated[i]){
    house_process$years[i]<-2020-house_process$yr_built[i]
  }else{
    house_process$years[i]<-2020-house_process$yr_renovated[i]
  }
}
house_process$yr_built<-NULL
house_process$yr_renovated<-NULL

test<-read.csv("/Users/Anesthesia/Desktop/Spring 2020/STAT502/class project/isu-stat-502-sp-2020/test.csv")
test$property<-NULL
test$date<-NULL
#test$date<-as.factor(test$date)
test$waterfront<-as.factor(test$waterfront)
test$zipcode<-as.factor(test$zipcode)
test$lat<-NULL
test$long<-NULL
test$waterfront<-as.factor(test$waterfront)
test$zipcode<-as.factor(test$zipcode)
test$view<-as.factor(test$view)
for(i in 1:10000){
  if(test$yr_built[i]>test$yr_renovated[i]){
    test$years[i]<-2020-test$yr_built[i]
  }else{
    test$years[i]<-2020-test$yr_renovated[i]
  }
}
test$yr_built<-NULL
test$yr_renovated<-NULL
alltest<-test