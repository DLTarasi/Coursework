#load ggplot for plotting results
library(ggplot2)
#load and inspect data
data(iris)
head(iris)
summary(iris)
class(iris)
#initialize model, test different ks and factors
iris_clusters <- kmeans(iris[,3:4], centers=3)
iris_clusters
#create table of clusters and species
Clusters <- as.factor(iris_clusters$cluster)
table(Clusters, iris$Species)
#plot petal length vs. width, color points by species
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
#plot petal length vs. width, color points by cluster
ggplot(iris, aes(Petal.Length, Petal.Width, color = Clusters)) + geom_point()

