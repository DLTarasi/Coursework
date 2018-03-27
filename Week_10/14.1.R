#packages
library(ggplot2)
library(reshape2)
library(VIM)
library(mice)
#set Directory to data path
setwd('/Users/dave/isye6501/Week_10')
#create table of crime data with headers
data<-read.csv("breast-cancer-wisconsin.data.txt", header = TRUE)
#Check class and inspect data
str(data)
head(data)
summary(data)
#column x1.3 has missing values as "?" - replace with "NA"
data$X1.3 <- as.character(data$X1.3)
data$X1.3[data$X1.3 == "?"] <- NA
data$X1.3 <- as.numeric(data$X1.3)
#use 'mice' package for imputation
methods(mice)
md.pattern(data)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
meanimp <- mice(data, method = 'mean')
#Impute missing values using mean, linear regression, and linear regression with perturbation
datamean <- complete(meanimp)
regimp <- mice(data, method = 'norm.predict')
datatreg <- complete(regimp)
regperimp <- mice(data, method = 'norm.nob')
dataregper <- complete(regperimp)
#Check accuracy of each method using SVM model

