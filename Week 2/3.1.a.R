library(data.table)
library("kknn")
#set Directory to data path
setwd('/Users/dave/isye6501/Week 2')
#create table of credit card data - no headers 
data<-read.table("credit_card_data.txt", header = FALSE)
#Check class and inspect data
class(data)
head(data)
summary(data)
set.seed(10) # Set seed used in sampling so that we can reproduce sample
#generate train/test split
# sample 85% of data and assign to train set. Assign remaining 15% to test set
sample <- sample.int(n = nrow(data), size = floor(.85*nrow(data)), replace = FALSE)
train <- data[sample, ]
test  <- data[-sample, ]
#use cv.kknn to to perform k-fold cross validation. Test variety of Ks. 7 was best accuracy at 86%
cv<-cv.kknn(formula=V11~.,data=train, k=7, kernel="rectangular", distance=2,scale=TRUE, kcv = 10)
#convert cv.kknn output to data table then round response variable to convert
#continuous variable to 0 or 1
cv = data.table(cv[[1]])
cv$yhat <- round(cv$yhat)
cv
# Cross Validation Accuracy - compare actual vs. predicted
table(cv$y == cv$yhat)
prop.table(table(cv$y == cv$yhat))


#final test with best hyperparameters
model<-kknn(V11~.,train,test,k=7,distance=2,kernel="rectangular")
#get predictions and convert from continuous reponse to binary by rounding to 0 or 1
predictions<-round(fitted(model))
predictions
# generate confusion matrix - 0,0 means model predicts 0 and actual was 0
confusion <- table(test[,11], predictions)
confusion
# Compute accuracy on val
accu <- sum(diag(confusion))/nrow(test)
accu

