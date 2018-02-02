#set Directory to data path
setwd('/Users/dave/isye6501/Week 4')
#create table temp data with headers 
data<-read.table("temps.txt", header = TRUE)
#Check class and inspect data, plot a few years
class(data)
head(data)
summary(data)
plot.ts(data[,2:5])
#convert each year in data to ts object
data<-as.vector(unlist(data[,2:21]))
ts <- ts(data, start = 1996, end = 2015, frequency=123)
ts
hw<-HoltWinters(ts, seasonal = c("multiplicative"),
                optim.start = c(alpha = .5, beta = .5, gamma = .5))
hw
plot(hw)
hw$gamma
seasonality<-hw$fitted[,4]
seasonality
cusum seasonality values