library(ggplot2)
library(outliers)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 3')
#create table of credit card data - no headers 
data<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(data)
head(data)
summary(data)
#boxplot of crime data points - three potential outliers on high side of data
g <- ggplot(data, aes(x='',Crime))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot",
       x="City",
       y="Crime")
#single sided grubs tests
?grubbs.test
grubbs.test(data$Crime, type = 10)
