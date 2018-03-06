#packages
library(ggplot2)
library(reshape2)
#set Directory to data path
setwd('/Users/dave/isye6501/Week_8')
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

#split into training and test data
set.seed(1)
spec = c(train = .8, test = .2)

g = sample(cut(
  seq(nrow(uscrimedata)), 
  nrow(uscrimedata)*cumsum(c(0,spec)),
  labels = names(spec)
))

uscrimedata = split(uscrimedata, g)

########Build Model
library(MASS)
initial <- lm(Crime~M,data=uscrimedata$train)
step<- stepAIC(initial, scope = Crime~M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + 
                 U2 + Wealth + Ineq + Prob + Time, direction="forward", trace = "TRUE")
step$anova
step <- stepAIC(step, direction="backward", trace = "TRUE")
step$anova # display results

########Predict
#create test city data frame - do not use So as it was not used when creating principal components
test_city <- c(M = 14.0,
               Ed = 10.0, Po1 = 12.0, 
               Po2 = 15.5,LF = 0.640,
               M.F = 94.0, Pop = 150,
               NW = 1.1, U1 = 0.120,
               U2 = 3.6, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04, 
               Time = 39.0)
test_city <- as.data.frame(t(test_city))
#predict crime level in test city
new_crime = predict(object=step, newdata = test_city)
new_crime
steppred<-predict(object=step, newdata=uscrimedata$test)
steppred
R2step <- 1 - (sum((uscrimedata$test$Crime-steppred)^2)/sum((uscrimedata$test$Crime-mean(steppred))^2))
R2step
