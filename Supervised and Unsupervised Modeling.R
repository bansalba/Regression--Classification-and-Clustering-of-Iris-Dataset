ggplot(data = iris) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length, color = Species))


ggplot(data = iris, mapping = aes(x=Petal.Width, y=Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth()

aa <- ggplot(iris, aes(Species, Petal.Length, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Length Box Plot", x = "Species")

bb <- ggplot(iris, aes(Species, Petal.Width, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Peal Width (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Width Box Plot", x = "Species")

cc <- ggplot(iris, aes(Species, Sepal.Width, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Width (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Sepal Width Box Plot", x = "Species")

dd <- ggplot(iris, aes(Species, Sepal.Length, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Sepal Length Box Plot", x = "Species")

grid.arrange(aa  + ggtitle(""),
             bb  + ggtitle(""),
             cc + ggtitle(""),
             dd + ggtitle(""),
             nrow = 2,
             top = textGrob("Sepal and Petal Box Plot", 
                            gp=gpar(fontsize=15)))

```{r message=FALSE, warning=FALSE, echo=FALSE}
iris %>% ggpairs(aes(color=Species, alpha=.4)) %>% ggplotly()
```
# fitting a K nearest Neighbor
set.seed(12825368)

#train - test Split
iris_sample <- sample(x = nrow(iris), size = nrow(iris)*0.80)
train <- iris[iris_sample,]
test <- iris[-iris_sample,]

train
#knn
Specie <- train$Species
Specie2 <- test$Species
Species_pred <- knn(train = train[-5], test = test[-5], cl = Specie, k=11, prob = TRUE)
knitr::kable(table(Species_pred,Specie2))

Specie_actual <- test$Species
table(Species_pred, Specie_actual)

#Computing the accuracy
mean(Species_pred == Specie_actual)

#Seeing how the neighbors voted
Species_prob <- attr(Species_pred,"prob")
head(Species_pred)
head(Species_prob)


##K Means Clustering
install.packages("factoextra")
library(factoextra)
model1 <- kmeans(iris[,1:4], centers = 3)
fviz_cluster(model1, data = scale(iris[,1:4]), geom = "point", stand = FALSE, frame.type = "norm")

cluster_model1 <- print(model1$cluster)
iris_cluster <- mutate(iris, cluster = model1$cluster)

head(iris_cluster)

zz <- ggplot(iris_cluster, aes(x = iris_cluster$Petal.Length, y = iris_cluster$Petal.Width, color = factor(cluster))) +
  geom_point()

yy <- ggplot(iris_cluster, aes(x = iris_cluster$Sepal.Width, y = iris_cluster$Sepal.Length, color = factor(cluster))) +
  geom_point()

grid.arrange(zz  + ggtitle(""),
             yy  + ggtitle(""),
             nrow = 1,
             top = textGrob("Sepal and Petal Visualization of Clustering", 
                            gp=gpar(fontsize=15)))


#Elbow method

k<-list()
for(i in 1:10){k[[i]]<-kmeans(iris[,1:4],i)$withinss}
m<-sapply(k,sum)
plot(1:10,m,type="b",ylab="wss(within sum square)",xlab="No. of clusters")
abline(v=3,lty=2)

#other short way for optimum number of clusters
fviz_nbclust(scale(iris[1:4]), kmeans, method = "wss")+geom_vline(xintercept=3, linetype=2)

#2.Silhouette Method
library(cluster)
sil<-list()
for(i in 1:10){k[[i]]<-kmeans(iris[,1:4],i,nstart=25)$cluster}
for(i in 1:10){sil[[i]]<-silhouette(k[[i]], dist(scale(iris[,1:4])))}
head(sil[[3]][,1:3], 3)
plot(sil[[3]])

#finding optimal clusters by knowing the maximum silhouette width for different k values
fviz_nbclust(scale(iris[1:4]), kmeans, method = "silhouette")

#To understand better
sil_width<-0
for(i in 2:10){sil_width[i]<-mean(sil[[i]] [,3])}
plot(1:10,sil_width,type="b",xlab="No. of clusters")
abline(v=2,lty=2)


# Hierarchical Clustering

distance <- dist(iris[,1:4], method="euclidean") 
hc <- hclust(distance, method="average")
cluster_assignment <- cutree(hc, k = 3)
cluster_avarage <- mutate(iris, cluster_a = cluster_assignment)
ggplot(cluster_avarage, aes(x = Petal.Length, y = Petal.Width, color= factor(cluster_a), main)) + geom_point() +
  labs(title = "Iris Hierarchical clustering with average linkage method", x = "Species")

distance <- dist(iris[,1:4], method="euclidean") 
hc <- hclust(distance, method="complete")
cluster_assignment <- cutree(hc, k = 3)
cluster_complete <- mutate(iris, cluster_c = cluster_assignment)
ggplot(cluster_complete, aes(x = Petal.Length, y = Petal.Width, color= factor(cluster_c), main)) + geom_point() +
  labs(title = "Iris Hierarchical clustering with complete linkage method", x = "Species")

distance <- dist(iris[,1:4], method="euclidean") 
hc <- hclust(distance, method="single")
cluster_assignment <- cutree(hc, k = 3)
cluster_single <- mutate(iris, cluster_s = cluster_assignment)
ggplot(cluster_single, aes(x = Petal.Length, y = Petal.Width, color= factor(cluster_s), main)) + geom_point() +
  labs(title = "Iris Hierarchical clustering with single linkage method", x = "Species")


#Dendograms

hc_a <- hclust(distance, method="average")
hc_c <- hclust(distance, method="complete")
hc_s <- hclust(distance, method="single")

par(mfrow = c(1,3))
plot(hc_c, main = 'Complete Linkage')
plot(hc_s, main = 'Single Linkage')
plot(hc_a, main = 'Average Linkage')

head(cluster_single)









