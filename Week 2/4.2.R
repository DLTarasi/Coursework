#load ggplot for plotting results
library(ggplot2)
#load and inspect data
data(iris)
head(iris)
summary(iris)
class(iris)
#initialize model
clusters <- kmeans(iris[,3:4], centers=3)
clusters

ggplot2(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
