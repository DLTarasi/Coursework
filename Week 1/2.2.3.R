#
library("kknn")
#set Directory to data path
setwd('/Users/dave/isye6501/Week 1')
#create table of credit card data - no headers 
data<-read.table("credit_card_data.txt", header = FALSE)
#Check class and inspect data
class(data)
head(data)
summary(data)
#use train.kknn to find best k value. train.kknn uses leave one out
#cross validation, avoiding the issue mentioned in the HW instructions
train.kknn(formula=V11~.,data=data,kmax=100, kernel="rectangular", distance=2,scale=TRUE)
#generate train/test split
set.seed(123) # Set seed used in sampling so that we can reproduce sample
# sample 80% of data and assign to test set. Assign remaining 20% to test.
sample <- sample.int(n = nrow(data), size = floor(.8*nrow(data)), replace = FALSE)
train <- data[sample, ]
test  <- data[-sample, ]
#create model
model<-kknn(V11~.,train,test,k=22,distance=2,kernel="rectangular")
#get predictions and convert from continuous reponse to binary by rounding to 0 or 1
predictions<-round(fitted(model))
predictions
# generate confusion matrix - 0,0 means model predicts 0 and actual was 0
confusion <- table(test[,11], predictions)
confusion
# Compute accuracy
accu <- sum(diag(confusion))/nrow(test)
accu
