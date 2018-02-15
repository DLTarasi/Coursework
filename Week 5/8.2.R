#packages
library(ggplot2)
library(outliers)
library(reshape2)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 5')
#create table of crime data with headers
uscrimedata<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(uscrimedata)
head(uscrimedata)
summary(uscrimedata)

######Outlier Testing - ultimately decided not to remove and didn't use this code.
#single sided grubs tests
grubbs.test(uscrimedata$Crime, type = 10)
# 1993 is an outlier at a p-value of .1, but not at .05 
#data<-data[-26,]
#grubbs.test(uscrimedata$Crime, type = 10)
# 1969 is also an outlier at p-value of .1 once 1993 is removed
#data<-data[-4,]
#grubbs.test(uscrimedata$Crime, type = 10)
#1674 is not an outlier at p-value of of .1

#####Plotting
#plot all variables
uscrime_melt = melt(data=uscrimedata, measure.vars = colnames(uscrimedata[,1:15]))
ggplot(data = uscrime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")

########Build Model
#Create model with all variables
basecrimemodel <- lm(Crime ~., uscrimedata)
summary(basecrimemodel)
AIC(basecrimemodel)
BIC(basecrimemodel)

#create model with only variables with p value below .1 - chose this one
crimemodelp.1 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, uscrimedata)
summary(crimemodelp.1)
AIC(crimemodelp.1)
BIC(crimemodelp.1)

#create model with only variables with p value below .05
crimemodelp.05 <- lm(Crime ~ M + Ed + Ineq + Prob, uscrimedata)
summary(crimemodelp.05)
AIC(crimemodelp.05)
BIC(crimemodelp.05)


##########check for multicolinearity
#create correlation matrix
cormat <- round(cor(uscrimedata),2)
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
#plot correlation matrix
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

######Remove cross correlated variables from p .1 model
#Try the above model without education (higher p value of two correlated variables ed and ineq) - results are worse, use original.
crimemodelp.1v2 <- lm(Crime ~ M + Po1 + U2 + Ineq + Prob, uscrimedata)
summary(crimemodelp.1v2)
AIC(crimemodelp.1v2)
BIC(crimemodelp.1v2)

########Test
#create test city data frame
test_city <- c(M = 14.0, So = 0,
               Ed = 10.0, Po1 = 12.0, 
               Po2 = 15.5,LF = 0.640,
               M.F = 94.0, Pop = 150,
               NW = 1.1, U1 = 0.120,
               U2 = 3.6, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04, 
               Time = 39.0)
test_city <- as.data.frame(t(test_city))

#predict crime level in test city using the crime model of variables with p value below .1- Result 1304.245 - higher than all but 5 of the samples
new_crime = predict(object=crimemodelp.1, newdata = test_city)



