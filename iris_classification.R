# GET DATA
data(iris)
head(iris)
summary(iris)

# EXPLORATORY ANALYSIS
# SEPAL PROPERTIES INTERACTIONS

library(ggplot2)
ggplot(iris,aes(x=Sepal.Width,y=Sepal.Length, shape=Species, color=Species)) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) +
  geom_point(size=2) + 
  labs(title = "Sepal Width Vs. Sepal Length")

# PETAL PROPERTIES INTERACTIONS
ggplot(iris,aes(x=Petal.Width,y=Petal.Length, shape=Species, color=Species)) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  geom_point(size=2) + 
  labs(title = "Petal Length Vs. Petal Width")

# So, Petal properties, seems to be an importantant parameter for our dataset
par(mfrow = c(2,2))
boxplot(iris$Sepal.Width~iris$Species)
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Petal.Width~iris$Species)
boxplot(iris$Petal.Length~iris$Species)

dev.off()

# PAIRWISE MATRIX FOR OUR DATA SET
pairs(iris[1:4],main = "Iris Data", pch = 21, bg = c("red", "green3", "blue"))

# Splitting the dataset into training and testing data
data(iris)
## set the seed to make your partition reproductible
set.seed(123)
n = nrow(iris)
train = sample(1:n, size = round(0.7*n), replace=T)
iris.train = iris[train,]
iris.test = iris[-train,]

summary(iris)
summary(iris.train)
summary(iris.test)




### Classification using NAIVE BAYES CLASSIFICATION

library(e1071)

nb_classifier <- naiveBayes(iris.train, factor(iris.train$Species), data = iris.train)
nb_pred <- predict(nb_classifier, iris.test[,-5])
table(nb_pred, iris.test[,5])

# nb_pred      setosa versicolor virginica

# setosa         26          0         0
# versicolor      0         19         0
# virginica       0          2        29





### CLASSIFICATION usin Decision Tree (CART)
library(rpart)
library(rpart.plot)
tree_model <- rpart(Species~., data = iris.train)
rpart.plot(tree_model)

library(party)
iris_tree <- ctree(Species~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_tree)
plot(iris_tree)




## Support Vector Machines (SVM)	

library(e1071)
svm_model <- svm(Species~., data = iris.train)
svm_predict <-predict(svm_model, iris.test[,-5])
table(iris.test$Species, svm_predict)
svm_predict

# setosa versicolor virginica
# setosa         26          0         0
# versicolor      0         19         2
# virginica       0          0        29

plot(svm_model,iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

















