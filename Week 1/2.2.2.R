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
# call ksvm using rbfdot - 99.5% accuracy, but almost certainly overfitting - 
# testing on the same data we trained on, and using a very high c value.
model <- ksvm(data[,1:10],data[,11],type="C-svc",kernel="rbfdot",C=10000,scaled=TRUE)
# calculate a1...am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
#plot a
plot(a)
# calculate a0
a0 <- -model@b
a0
# see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)



