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
