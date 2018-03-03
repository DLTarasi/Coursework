
#set Directory to data path
setwd('/Users/dave/isye6501/Week 7')
#create table of crime data with headers
gcdata<-read.table("germancredit.txt", header = FALSE)
#Check class and inspect data
class(gcdata)
head(gcdata)
summary(gcdata)
#convert response column from 1 and 2 into 0 and 1 for logistic regression
twotozero<-function(x){
    if (x == 2)
      x<-0
      return(x)
}
gcdata$V21 <- sapply(gcdata$V21, twotozero)

#CREATE TRAIN TEST VAL SPLIT
set.seed(1)
spec = c(train = .7, test = .15, validate = .15)

g = sample(cut(
  seq(nrow(gcdata)), 
  nrow(gcdata)*cumsum(c(0,spec)),
  labels = names(spec)
))

gcdata = split(gcdata, g)


#build model - choose based on AIC - 706 with all factors
logreg<-glm(V21~V1+V2+V3+V4+V5+V6+V8+V14, data = gcdata$train, family=binomial(link='logit')) #best model
summary(logreg)

#choosing a threshold based on loss function
pred<-predict(logreg, gcdata$validate)
pred
l = c()
thresholds=c()
thresh<-.1
bin_pred = as.integer(pred>(thresh))
confusion_matrix = table(bin_pred,gcdata$validate$V21)
loss = confusion_matrix[2,1] + 5*confusion_matrix[1,2]
l<-c(l,loss)
thresholds<-c(thresholds,thresh)
confusion_matrix
loss
plot(l, x = thresholds)
#predict 
pred_test<-predict(logreg, gcdata$test)
bin_pred_test = as.integer(pred_test>(.1))
confusion_matrix_test = table(bin_pred,gcdata$test$V21)
loss = confusion_matrix_test[2,1] + 5*confusion_matrix_test[1,2]
confusion_matrix_test
loss
