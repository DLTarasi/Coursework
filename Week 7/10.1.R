#packages
library(rpart)
library(rpart.plot)
library(ggplot2)
library(reshape2)
library(randomForest)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 7')
#create table of crime data with headers
uscrimedata<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(uscrimedata)
head(uscrimedata)
summary(uscrimedata)

#####Plotting and Data Cleaning - did not remove potential outliers based on week three analysis
#plot all variables
uscrime_melt = melt(data=uscrimedata, measure.vars = colnames(uscrimedata[,1:15]))
ggplot(data = uscrime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")

#Combine P01 and p02 into single variable that is the average of both - they are very similar and probably should not be splitting trees on both
uscrimedata$Po<-(uscrimedata$Po1 + uscrimedata$Po2)/2
uscrimedata <- uscrimedata[,-4]
uscrimedata <- uscrimedata[,-4]

#CREATE TRAIN TEST SPLIT
set.seed(1)
spec = c(train = .7, test = .3)

g = sample(cut(
  seq(nrow(uscrimedata)), 
  nrow(uscrimedata)*cumsum(c(0,spec)),
  labels = names(spec)
))

uscrimedata = split(uscrimedata, g)

########Build Model
#create tree model
?rpart
rtree<-rpart(Crime ~M + Ed + Po + Wealth + Ineq + Prob, uscrimedata$train, xval = 100, minbucket = 2)
rpart.plot(rtree)
summary(rtree)
rtpredtrain<-predict(rtree, uscrimedata$train)
R2RT <- 1 - (sum((uscrimedata$train$Crime-rtpredtrain )^2)/sum((uscrimedata$train$Crime-mean(rtpredtrain))^2))
R2RT
#create randomforest model
?randomForest
rforest<-randomForest(Crime ~ M + Ed + Po + Wealth + Ineq + Prob, uscrimedata$train, xtest = test_city, keep.forest = TRUE)
summary(rforest)
rforest$importance
rfpredtrain<-predict(rforest, uscrimedata$train)
rfpredtrain
R2RF <- 1 - (sum((uscrimedata$train$Crime-rfpredtrain)^2)/sum((uscrimedata$train$Crime-mean(rfpredtrain))^2))
R2RF
########Predict
#create test city data frame with the 6 components used in the model
test_city <- c(M = 14.0,Ed = 10.0, 
               Po = 13.75, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04)
test_city <- as.data.frame(t(test_city))


#regression tree
predict(rtree, test_city)
rtpredtest<-predict(rtree, uscrimedata$test)
rtpredtest
R2RTTest <- 1 - (sum((uscrimedata$test$Crime-rtpredtest )^2)/sum((uscrimedata$test$Crime-mean(rtpredtest))^2))
R2RTTest

#random forest
rforest$test
rfpredtest<-predict(rforest, uscrimedata$test)
rfpredtest
R2RFTest <- 1 - (sum((uscrimedata$test$Crime-rfpredtest)^2)/sum((uscrimedata$test$Crime-mean(rfpredtest))^2))
R2RFTest

