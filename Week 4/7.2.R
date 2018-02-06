library("qcc")
#set Directory to data path
setwd('/Users/dave/isye6501/Week 4')
#create table temp data with headers 
data<-read.table("temps.txt", header = TRUE)
#Check class and inspect data, plot a few years
class(data)
head(data)
summary(data)
plot.ts(data[,2:5])
#convert each year in data to timeseries object
data<-as.vector(unlist(data[,2:21]))
ts <- ts(data, start = 1996, frequency=123)
ts
#run HoltWinters on the timeseries, using Alpha, Beta, and Gamma, multiplicative seasonality to try to capture non-linearity in data
hw<-HoltWinters(ts, seasonal = c("multiplicative"))
hw
#Plot the smoothed data
plot(hw)
#see what the cooeficient for gamma is
hw$gamma
#plot the fitted factors for the model
plot(hw$fitted)
#store the value of the seasonality adjustment in a single vector.
seasonality<-hw$fitted[,4]
#plot seasonality adjustment -increases in later years.
plot(seasonality)
# export all data 
write.table(seasonality, "/Users/dave/isye6501/Week 4/seasonality.txt", sep="\t")
seasonality_by_year <- matrix(hw$fitted[,4],ncol=19)
write.table(seasonality_by_year, "/Users/dave/isye6501/Week 4/seasonality_by_year.txt", sep="\t")
