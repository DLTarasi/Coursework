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
#convert to matrix for use with glm 
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
?glmnet
#base elastic net
elnet <- glmnet(x$train, y$train, family="mgaussian", alpha=.5, standardize = "TRUE")
#ridge
ridge <- glmnet(x$train, y$train, family="mgaussian", alpha=0, standardize = "TRUE")

# Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x$train, y$test, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x$test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x$test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x$test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x$test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x$test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x$test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x$test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x$test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x$test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x$test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x$test)

mse0 <- mean((y$test - yhat0)^2)
mse1 <- mean((y$test - yhat1)^2)
mse2 <- mean((y$test - yhat2)^2)
mse3 <- mean((y$test - yhat3)^2)
mse4 <- mean((y$test - yhat4)^2)
mse5 <- mean((y$test - yhat5)^2)
mse6 <- mean((y$test - yhat6)^2)
mse7 <- mean((y$test - yhat7)^2)
mse8 <- mean((y$test - yhat8)^2)
mse9 <- mean((y$test - yhat9)^2)
mse10 <- mean((y$test - yhat10)^2)

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
test_city <-t(test_city)
#predict crime level in test city
new_crime = predict(object=elnet, newdata = test_city)
new_crime
elpred<-predict(object=elnet, newdata=x$test)
elpred
R2EL <- 1 - (sum((x$test$Crime-elpred )^2)/sum((x$test$Crime-mean(elpred))^2))
R2EL


