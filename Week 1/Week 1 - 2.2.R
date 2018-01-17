#run kernlab
library("kernlab")
#Set Directory to data path
setwd('/Users/dave/isye6501/Week 1')
#create table of credit card data - no headers 
data<-read.table("credit_card_data.txt", header = FALSE)
#Check class and inspect data
class(data)
head(data)
summary(data)
#convert from df to matrix
data<-as.matrix(data)
class(data)
# call ksvm.  Vanilladot is a simple linear kernel. Changing C changes tradeoff
# between error and margin. Because changes in C values over a large range (between
# .001 and 100,000) have minimal impact on the models predictions, we can guess
# that the data is likely well seperated and we don't have to make much tradeoff
# between a large margin and avoiding mistakes.
model <- ksvm(data[,1:10],data[,11],type="C-svc",
              kernel="vanilladot",C=100000,scaled=TRUE)
#choose 100,000 for c because it is within the range giving minimum error rate at maximum margin.
# calculate a1...am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
#plot each a value - it is clear from this plot that column 5 of the data is by far the 
# most predictive.
plot(a)
#create confusion matrix of attribute 5 and results - from this matrix it appears 
# that attribute 5 is deterimative in about 86% of cases (286+278/654=.862). 
# This is very close to what our SVM predicts.
table(data[,11], data[,5])
# calculate a0
a0 <- -model@b
a0
# see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)
# calculate margin for model
margin = 2/sqrt(sum(a^2))
margin

