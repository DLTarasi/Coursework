#set Directory to data path
setwd('/Users/dave/isye6501/Week 7')
#create table of crime data with headers
gcdata<-read.table("germancredit.txt", header = FALSE)
#Check class and inspect data
class(gcdata)
head(gcdata)
summary(gcdata)

#NEED TO CREATE TRAIN TEST SPLIT

#convert response column from 1 and 2 into 0 and 1 for logistic regression
twotozero<-function(x){
    if (x == 2)
      x<-0
      return(x)
}
gcdata$V21 <- sapply(gcdata$V21, twotozero)
#build model
?glm
logreg<-glm(V21~., data = gcdata, family=binomial(link='logit'))
summary(logreg)
