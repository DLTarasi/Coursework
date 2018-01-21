#load ggplot and ggalt for plotting results
library(ggplot2)
library(ggalt)
#load and inspect data
data(iris)
head(iris)
summary(iris)
class(iris)
#plot relationships between combo of predictors based on thise plot it looks
#like petal length and petal width are best predictors
plot(iris)
#initialize model, test different ks and predictors
iris_cluster_model <- kmeans(iris[,3:4], centers=3)
iris_cluster_model
#create table of clusters and species
Clusters <- as.factor(iris_cluster_model$cluster)
Clusters
table(Clusters, iris$Species)
#plot petal length vs. width, color points by species
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
#plot petal length vs. width, color points by cluster - very close to species 
ss1<-iris[1:50,]
ss2<-subset(iris, iris_cluster_model$cluster == 2)
ss3<-subset(iris, iris_cluster_model$cluster == 3)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Clusters)) + geom_point() + 
       geom_encircle(data=ss1, s_shape=0.5, expand=0.0, colour="red") + 
       geom_encircle(data=ss2, s_shape=0.5, expand=0.0, colour="green") +
       geom_encircle(data=ss3, s_shape=0.5, expand=0.0, colour="blue")



