library(ggplot2)
library(outliers)
library(reshape2)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 5')
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

#plot all variables
uscrime_melt = melt(data=uscrimedata, measure.vars = colnames(uscrimedata[,1:15]))
ggplot(data = uscrime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")
#Create model with all variables
crimemodel <- lm(Crime ~., uscrimedata)
summary(crimemodel)
AIC(crimemodel)
#create model with only variables with p value below .1
crimemodel <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, uscrimedata)
summary(crimemodel)
AIC(crimemodel)
#create test city data frame
test_city <- c(M = 14.0, So = 0,
               Ed = 10.0, Po1 = 12.0, 
               Po2 = 15.5,LF = 0.640,
               M.F = 94.0, Pop = 150,
               NW = 1.1, U1 = 0.120,
               U2 = 3.6, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04, 
               Time = 39.0)
test_city <- as.data.frame(t(test_city))
#predict crime level in test city using the crime model
new_crime = predict(object=crimemodel, newdata = test_city)


