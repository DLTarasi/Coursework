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
uscrimedata <- as.matrix(uscrimedata)
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
########Build Model
lasso <- glmnet(x, y, family="mgaussian", alpha=1, standardize = "TRUE")
cv_fit <- cv.glmnet(x, y, alpha = 1)
plot(cv_fit)
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
test_city <- t(test_city)
#predict crime level in test city
new_crime = predict(object=lasso, newdata = test_city)
new_crime
lassopred<-predict(object=lasso, newdata=x$test)
lassopred
R2lasso <- 1 - (sum((x$test$Crime-lassopred )^2)/sum((x$test$Crime-mean(lassopred))^2))
R2lasso

