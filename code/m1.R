dat<-read.csv("/Users/Anesthesia/Desktop/Spring 2020/STAT502/class project/isu-stat-502-sp-2020/train.csv")
house<-dat
house$property<-NULL

date<-house$date #category
price<-house$price
numofBeds<-house$bedrooms
numofBaths<-house$bathrooms
sqft_living<-house$sqft_living
sqft_lot<-house$sqft_lot
floors<-house$floors
waterfront<-house$waterfront #category
view<-house$view
condition<-house$condition
grade<-house$grade
sqft_above<-house$sqft_above 
sqft_basement<-house$sqft_basement
yr_built<-house$yr_built
yr_renovated<-house$yr_renovated
zipcode<-house$zipcode #category
sqft_living15<-house$sqft_living15
sqft_lot15<-house$sqft_lot15

house_process<-house
house_process$date<-as.factor(house$date)
house_process$waterfront<-as.factor(house$waterfront)
house_process$zipcode<-as.factor(house$zipcode)
m1<-lm(price~.,data=house_process)
rmsle<-function(predictions,actual){
  t<-sum((log(predictions+1)-log(actual+1))^2)
  return(sqrt(t/length(actual)))
}
rmsle(abs(m1$fitted.values),house$price)

####generate test file for kaggle submission
test<-read.csv("/Users/Anesthesia/Desktop/Spring 2020/STAT502/class project/isu-stat-502-sp-2020/test.csv")
test$property<-NULL
test$date<-NULL
#test$date<-as.factor(test$date)
test$waterfront<-as.factor(test$waterfront)
test$zipcode<-as.factor(test$zipcode)
test.pred<-predict.lm(m1,test)
test.dat<-cbind(1:11613,abs(test.pred))
colnames(test.dat)<-c("id","price")
write.csv(test.dat,"/Users/Anesthesia/Desktop/Spring 2020/STAT502/class project/test1.csv",row.names = F)

###cross  validation
folds <- split(sample(nrow(house_process),nrow(house_process),replace=FALSE),as.factor(1:5))
set.seed(1234)
ctrl <- trainControl(method = "cv", number = 10, savePred = T)
fold.accuracy.estimate <- NULL
for(f in 1:5){
  trainingData <- house_process[-folds[[f]],]
  testData <- house_process[folds[[f]],]
  model_lm <- train(price~., data=trainingData, method="lm",trControl=ctrl)
fold.accuracy.estimate[f] <- rmsle(abs(model_lm$pred$pred),model_lm$pred$obs)
}
mean(fold.accuracy.estimate) 
