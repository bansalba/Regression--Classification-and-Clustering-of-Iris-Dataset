#####MACY#####

#Part I: Exploratory Data Analysis
summary(iris)
str(iris)
aggregate(.~Species, iris, mean)
library(psych)
pairs.panels(iris)

#Histograms of sepal and petal info, default breaks hide some games
hist(iris$Sepal.Length, prob=T, col="blue", breaks=10, main="Histogram and Density of Sepal Length", xlim=c(3,9), xlab="Sepal Length")
lines(density(iris$Sepal.Length), col="yellow", lwd=2)
hist(iris$Sepal.Width, prob=T, col="blue", breaks=10, main="Histogram and Density of Sepal Width", xlim=c(1,5), xlab="Sepal Wdith") #has the most normal distribution
lines(density(iris$Sepal.Width), col="yellow", lwd=2)
hist(iris$Petal.Length, prob=T, col="blue", breaks=10, main="Histogram and Density of Petal Length", xlim=c(0,7), xlab="Petal Length")
lines(density(iris$Petal.Length), col="yellow", lwd=2)
hist(iris$Petal.Width, prob=T, col="blue", breaks=10, main="Histogram and Density of Petal Width", xlim=c(0,3), xlab="Petal Width")
lines(density(iris$Petal.Width), col="yellow", lwd=2)

boxplot(iris[,1:4], notch=T, col=c("green", "orange", "purple", "blue"))
boxplot(iris[,1]~iris[,5], notch=T, ylab="Sepal Length", col="blue")
boxplot(iris[,2]~iris[,5], notch=T, ylab="Sepal Width", col="blue")
boxplot(iris[,3]~iris[,5], notch=T, ylab="Petal Length", col="green")
boxplot(iris[,4]~iris[,5], notch=T, ylab="Petal Width", col="purple")

ggplot(iris, aes(x=iris$Sepal.Length, y=iris$Sepal.Width, color=iris$Species)) + geom_point()
ggplot(iris, aes(x=iris$Petal.Length, y=iris$Petal.Width, color=iris$Species)) + geom_point()

#Sepal Length vs Width, No correlation Adj-R=0.005
sepal<-lm(iris$sepal_width~iris$sepal_length)
summary(sepal)
plot(iris$sepal_length, iris$sepal_width)
abline(sepal, col="red")

#Petal Length vs Width, stronger relationship Adj-R=0.9264
Petal<-lm(iris$Petal.Width~iris$Petal.Length)
plot(iris$Petal.Length, iris$Petal.Width)
abline(Petal, col="purple")
summary(Petal)


#Part II: Surpervised Learning

setosa<- rbind(iris[iris$Species=="setosa",])
versicolor<- rbind(iris[iris$Species=="versicolor",])
virginica<- rbind(iris[iris$Species=="virginica",])
ind<- 1:30
iris_train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,])
iris_test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])

install.packages("class")
library(class)
knn_iris <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=8)
knn_iris

#k=70
table(iris_test[,5], knn_iris, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris)

#KUNAL#

#iris dataset
head(iris)
dim(iris)
summary(iris)

# mean,median,variance
aggregate(.~Species,iris,mean)
aggregate(.~Species,iris,median)
var(iris$Sepal.Length)

#histogram

hist(iris$Petal.Width, prob=T, col="blue", breaks=10, main="Histogram and Density of Petal Width", xlim=c(0,3), xlab="Petal Width")
lines(density(iris$Petal.Width), col="red", lwd=2)

# scatter plot
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, colour = Species, data = iris)
qplot(Petal.Length, Petal.Width, colour = Species, data = iris)

#knn 
set.seed(12825368)
iris_sample <- sample(x = nrow(iris), size = nrow(iris)*0.80)
iris_train <- iris[iris_sample,]
iris_test <- iris[-iris_sample,]

install.packages("class")
knn_iris <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=11)
knn_iris
#prediction
table(iris_test[,5], knn_iris, dnn = c("Actual", "Predicted"))



# K means
install.packages("fpc")
library(fpc)

fit <- kmeans(iris[,1:4], 3)
plotcluster(iris[,1:4], fit$cluster)
fit <- kmeans(iris[,1:4], 5)
plotcluster(iris[,1:4], fit$cluster)

fit <- kmeans(iris[,1:4], 7)
plotcluster(iris[,1:4], fit$cluster)

# hiearchical clustering
hc_result <- hclust(dist(iris[,1:4]))
plot(hc_result)
#Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k=3)
rect.hclust(hc_result, k=5)
rect.hclust(hc_result, k=7)


