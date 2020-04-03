housing<-read.csv("./data/isu-stat-502-sp-2020/train.csv")
house<-housing[,3:21]
#The model GAM
#loding packages
library(caret)

smoothhousing<-train(price~.,data=house,
                     preProcess =c("center","scale"),
                     tunegrid=data.frame(df=1:18),method="gamSpline",
                     trControl=trainControl(method="repeatedcv", repeats=10,number=10))

HouseTest<-read.csv("./data/isu-stat-502-sp-2020/test.csv")
HouseTest<-HouseTest[,3:21]


Model2<-predict(smoothhousing, house)

SELa<- c()
SELa<-c(SELa, sum(Model2-house[,1])^2)
CVRMSPEa<-sqrt(sum(SELa)/nrow(house))

test.pred2<-predict(smoothhousing,HouseTest)
test.House<-cbind(1:11613,test.pred2)
colnames(test.House)<-c("id","price")
write.csv(test.House,"./predictions/test2.csv",row.names = F)
