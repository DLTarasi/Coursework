library(ggplot2)
library(outliers)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 3')
#create table of crime data with headers
data<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(data)
head(data)
summary(data)
#boxplot of crime data points - three potential outliers on high side of data
g <- ggplot(data, aes(x='',Crime))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(x="Cities",
       y="Crime")
#single sided grubs tests
grubbs.test(data$Crime, type = 10)
# 1993 is an outlier at a p-value of .1, but not at .05
data<-data[-26,]
grubbs.test(data$Crime, type = 10)
# 1969 is also an outlier at p-value of .1 once 1993 is removed
data<-data[-4,]
grubbs.test(data$Crime, type = 10)
#1674 is not an outlier at p-value of of .1
ggplot(data) + geom_point(aes(x=U2, y=Crime))
model <- lm(Crime ~., data)
summary(model)
model <- lm(Crime ~ Ed + U2 + Ineq + Prob, data)
summary(model)
