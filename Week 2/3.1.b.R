library("kknn")
#set Directory to data path
setwd('/Users/dave/isye6501/Week 2')
#create table of credit card data - no headers 
data<-read.table("credit_card_data.txt", header = FALSE)
#Check class and inspect data
class(data)
head(data)
summary(data)
#generate train/test/val split
set.seed(10) # Set seed used in sampling so that we can reproduce sample
# sample 70% of data and assign to train set. Split remaining 30% 1/2 to validation
# and 1/2 to test set (so 15% of total to validation, 15% of total data set to test).
sample <- sample.int(n = nrow(data), size = floor(.7*nrow(data)), replace = FALSE)
sample2 <- sample.int(n = nrow(data[-sample,]), size = floor(.5*nrow(data[-sample,])), replace = FALSE)
train <- data[sample, ]
val <- data[sample2,]
test  <- data[-sample, ][-sample2,]
#create model - try a variety of hyperparameters using the validation set
model<-kknn(V11~.,train,val,k=13,distance=2,kernel="rectangular")
#get predictions and convert from continuous reponse to binary by rounding to 0 or 1
predictions<-round(fitted(model))
predictions
# generate confusion matrix - 0,0 means model predicts 0 and actual was 0
confusion <- table(val[,11], predictions)
confusion
# Compute accuracy on val
accu <- sum(diag(confusion))/nrow(val)
accu

#final test with best hyperparameters
model<-kknn(V11~.,train,test,k=13,distance=2,kernel="rectangular")
#get predictions and convert from continuous reponse to binary by rounding to 0 or 1
predictions<-round(fitted(model))
predictions
# generate confusion matrix - 0,0 means model predicts 0 and actual was 0
confusion <- table(test[,11], predictions)
confusion
# Compute accuracy on val
accu <- sum(diag(confusion))/nrow(test)
accu

