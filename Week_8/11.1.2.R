#packages
library(glmnet)
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
#convert to matrix for use with glm and split into input and response vars
x<-uscrimedata[,1:15]
y<-uscrimedata[,16]
#MAY NEED TO SCALE
#split into training and test set
set.seed(1)
spec = c(train = .8, test = .2)

g = sample(cut(
  seq(nrow(x)), 
  nrow(x)*cumsum(c(0,spec)),
  labels = names(spec)
))

x = split(x,g)
y = split(y,g)

x.train <- as.matrix(x$train)
x.test <- as.matrix(x$test)

y.train <- as.matrix(y$train)
y.test <- as.matrix(y$test)
class(y.test)########Build Model
?glmnet
lasso <- glmnet(x.train, y.train, family="mgaussian", alpha=1)
cv_fit <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv_fit)
########Predict
#create test city data frame
test_city <- c(M = 14.0, So = 1,
               Ed = 10.0, Po1 = 12.0, 
               Po2 = 15.5,LF = 0.640,
               M.F = 94.0, Pop = 150,
               NW = 1.1, U1 = 0.120,
               U2 = 3.6, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04, 
               Time = 39.0)
test_city <- t(test_city)
#predict crime level in test city
new_crime = predict(object=cv_fit, newx = test_city)
new_crime
lassopred<-predict(object=cv_fit, newx=x.test)
lassopred
R2lasso <- 1 - (sum((y.test-lassopred )^2)/sum((y.test-mean(lassopred))^2))
R2lasso

