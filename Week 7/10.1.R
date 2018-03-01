#packages
library(pls)
library(rpart)
library(ggplot2)
library(reshape2)
library(randomForest)
library(ggfortify)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 7')
#create table of crime data with headers
uscrimedata<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(uscrimedata)
head(uscrimedata)
summary(uscrimedata)

#####Plotting and Data Cleaning
#####Outlier Testing - did not remove potential outliers based on week three analysis
#plot all variables
uscrime_melt = melt(data=uscrimedata, measure.vars = colnames(uscrimedata[,1:15]))
ggplot(data = uscrime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")
#Combine P01 and p02 into single variable that is the average of both - they are very similar and probably should not be splitting trees on both
uscrimedata$Po<-(uscrimedata$Po1 + uscrimedata$Po2)/2
uscrimedata <- uscrimedata[,-4]
uscrimedata <- uscrimedata[,-4]

########Build Model
#create tree model
?rpart
rtree<-rpart(Crime ~., uscrimedata)
summary(rtree)
#create randomforest model
?randomForest
rforest<-randomForest(Crime ~., uscrimedata, xtest = test_city)
summary(rforest)
plot(rforest)

########Predict
#create test city data frame - do not use So as it was not used when creating principal components
test_city <- c(M = 14.0,Ed = 10.0, 
               Po = 13.75,LF = 0.640,
               M.F = 94.0, Pop = 150,
               NW = 1.1, U1 = 0.120,
               U2 = 3.6, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04, 
               Time = 39.0, So = 0)
test_city <- as.data.frame(t(test_city))
#regression tree
predict(rtree, test_city)

#random forest
rforest$test

