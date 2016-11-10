


# Exploring the data

library(datasets)
head(iris)
# From the data, we can find that there is a good relation b/w petal length and petal width
cor(iris[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
summary(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()


# Okay, now we have seen the data, let us try to cluster it.
# let us seed the data, to increase reproducibility

set.seed(20)
irisCluster <- kmeans(iris[,3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)


irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()