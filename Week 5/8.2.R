library(ggplot2)
library(outliers)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 3')
#create table of crime data with headers
uscrimedata<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(uscrimedata)
head(uscrimedata)
summary(uscrimedata)

#single sided grubs tests
grubbs.test(uscrimedata$Crime, type = 10)
# 1993 is an outlier at a p-value of .1, but not at .05
#data<-data[-26,]
#grubbs.test(uscrimedata$Crime, type = 10)
# 1969 is also an outlier at p-value of .1 once 1993 is removed
#data<-data[-4,]
#grubbs.test(uscrimedata$Crime, type = 10)
#1674 is not an outlier at p-value of of .1
ggplot(data) + geom_point(aes(x=U2, y=Crime))
model <- lm(Crime ~., data)
summary(model)
model <- lm(Crime ~ Ed + U2 + Ineq + Prob, data)
summary(model)

test_city <- c()

new_crime = predict(object=model, newdata = test_city)

uscrime_melt = melt(data=uscrime, measure.vars = colnames(uscrime[,1:15]))

citycrime_melt = melt(data=citycrime, measure.vars = colnames(citycrime[,1:15]))

ggplot(data = uscrime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  geom_point(data=citycrime_melt, color = "red") +
  facet_wrap(~variable, scales = "free")
