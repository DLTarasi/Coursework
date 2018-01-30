library(reshape2)
library(ggplot2)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 3')
#create table temp data with headers 
data<-read.table("temps.txt", header = TRUE)
#Check class and inspect data, plot a few years
class(data)
head(data)
summary(data)
plot.ts(data[,2:5])

