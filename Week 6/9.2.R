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
plot(pcamodel)
plot(pcamodel$loadings)
variancepercomp<-pcamodel$Xvar/sum(pcamodel$Xvar)
#plot variance explained by each PC
plot(variancepercomp,
    xlab = "Principal Component",
    ylab = "Percent of Variance Explained")
#Plot Cumulative variance explained for certain number of PCs
plot(cumsum(variancepercomp),
     xlab = "Principal Component",
     ylab = "Percent of Variance Explained")
# calculate root mean squared error and r2 of prediction - 5 pcs looks best
plot(RMSEP(pcamodel))
plot(R2(pcamodel))
corrplot(pcamodel, 1:5)
loadingplot(pcamodel, 1:5, legendpos = "bottomleft")
pcamodel5 <- pcr(Crime ~., data = uscrimedata, scale = TRUE, validation = "LOO", ncomp = 5)

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
#predict crime level in test city using the crime model with 5 principal components - 1443
new_crime = predict(object=pcamodel5, newdata = test_city, ncomp = 5)
#convert PCs back to original coefficients, then unscale
pca5<-t(as.data.frame(coef(pcamodel5, intercept = TRUE)))
pca5coef<-pca5[2:15]/pcamodel5$scale
pca5
#get intercept from pcamodel5
intercept <- pca5[1]
#manually caclculate Crime rate for test city using intercept, unscaled coefficients and test city - confirm matches 1443
intercept + sum(pca5coef * test_city)

