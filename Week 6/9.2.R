#packages
library(pls)
library(ggplot2)
library(reshape2)
library(ggfortify)
#set Directory to data path
setwd('/Users/dave/isye6501/Week 6')
#create table of crime data with headers
uscrimedata<-read.table("uscrime.txt", header = TRUE)
#Check class and inspect data
class(uscrimedata)
head(uscrimedata)
summary(uscrimedata)



#####Plotting and Data Cleaning
#####Outlier Testing - did not remove potential outliers based on week three analysis
#plot all variables
uscrime_melt = melt(data=uscrimedata, measure.vars = colnames(uscrimedata[,1:15]))
ggplot(data = uscrime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")
#Remove So because it is categorical - PCA works best on continuos numerical data
uscrimedata <- uscrimedata[,-2]

########Build Model
#Find all principal components of dataset, using leave one out cv and scaling data
pcamodel <- pcr(Crime ~., data = uscrimedata, scale = TRUE, validation = "LOO")
summary(pcamodel)
pcacoef<-as.data.frame(coef(pcamodel))
# calculate mean squared error and r2 of prediction
MSEP(pcamodel)
R2(pcamodel)
#Create model with top 5 components 
pcamodel5 <- pcr(Crime ~., data = uscrimedata, scale = TRUE, ncomp = 5, validation = "LOO")
summary(pcamodel5)
pcacoef5<-t(as.data.frame(coef(pcamodel5)))
# calculate mean squared error and r2 of prediction
MSEP(pcamodel5)
R2(pcamodel5)

########Predict
#create test city data frame - do not use So as it was not used when creating principal components
test_city <- c(M = 14.0,
               Ed = 10.0, Po1 = 12.0, 
               Po2 = 15.5,LF = 0.640,
               M.F = 94.0, Pop = 150,
               NW = 1.1, U1 = 0.120,
               U2 = 3.6, Wealth = 3200,
               Ineq = 20.1, Prob = 0.04, 
               Time = 39.0)
test_city <- as.data.frame(t(test_city))
#predict crime level in test city using the crime model with X principal components
new_crime = predict(object=pcamodel5, newdata = test_city, ncomp = 5)

####### convert back to original vars
pca_output$rotation %*% lm_model_from_pca$coefficients[-1]
