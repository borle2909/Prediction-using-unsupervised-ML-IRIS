install.packages("ggcorrplot")
install.packages("GGally")
install.packages("ggplot2")
install.packages("gridExtra")
library(ggcorrplot)
library(GGally)
library(graphics)
library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
library(gridExtra)

iris<- read.csv("D:\spark foundation tasks\Iris.csv")
head(iris)

#iris<-subset(iris ,select=-c(Id))

str(iris)

summary(iris)

#checking for NAs
table(is.na(iris))

#Visualization the data frame
options(repr.plot.width=7,repr.plot.height=4)
plot(iris,col=iris$Species)

#Visualization sepal length vs sepal width
ggplot(iris)+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
  facet_wrap(~ Species)+
  theme_bw()

#Visualization petal length vs petal width
ggplot(iris)+
  geom_point(aes(x=Petal.Length,y=Petal.Width,color=Species))+
  facet_wrap(~ Species)+
  theme_bw()


#correlation plot of data
ggcorrplot(cor(iris[,1:4]),lab=TRUE)

#Pairs plot of data
options(repr.plot.width=7, repr.plot.height=5)
ggpairs(data=iris,column=1:4,aes(col=Species))

#Elbow method using within cluster sum of square (wcss)
set.seed(234)
iris_k <- iris[,1:4]
iris_k <- as.matrix(iris_k)
k_max <-6
wss <- sapply(1:k_max,function(k){kmeans(iris_k,k,iter.max=100,nstart=10)$tot.withinss})
wss

#finding optimum number of cluster by visualizing the Elbow graph
options(repr.plot.width=4,repr.plot.height=4)
plot(1:k_max,wss,type="b",xlab="No. of clusters")

#Forming the cluster using k-means clustering
iris_cluster<- kmeans(iris_k,3,nstart=10,iter.max=100)
iris_cluster

#Visualizing the cluster
options(repr.plot.width=7,repr.plot.height=5)
clusplot(iris_k,iris_cluster$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,
         main="Clusters of IRIS dataset")

#Combining the cluster number allocated to the iris dataset
clustered_data<- cbind(iris,cluster_number=iris_cluster$cluster)
head(clustered_data)

table(clustered_data$Species,clustered_data$cluster_number)

clustered_data$cluster_number <- as.factor(clustered_data$cluster_number)

#comparing sepal length vs sepal width plot
a<- iris %>% ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+geom_point()+theme(legend.position="bottom")
b<- clustered_data %>% ggplot(aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point(aes(color=cluster_number))+
  geom_point(aes(x=iris_cluster$centers[1,1],y=iris_cluster$centers[1,2]),pch=24,size=3,fill="black")+
  geom_point(aes(x=iris_cluster$centers[2,1],y=iris_cluster$centers[2,2]),pch=24,size=3,fill="black")+
  geom_point(aes(x=iris_cluster$centers[3,1],y=iris_cluster$centers[3,2]),pch=24,size=3,fill="black")+
  theme(legend.position= "bottom")

options(repr.plot.width=8,repr.plot.height=4)
grid.arrange(a,b,ncol=2)

#comparing petal length vs petal width plot
c<- iris %>% ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species))+geom_point()+theme(legend.position="bottom")
d<- clustered_data %>% ggplot(aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=cluster_number))+
  geom_point(aes(x=iris_cluster$centers[1,3],y=iris_cluster$centers[1,4]),pch=24,size=3,fill="black")+
  geom_point(aes(x=iris_cluster$centers[2,3],y=iris_cluster$centers[2,4]),pch=24,size=3,fill="black")+
  geom_point(aes(x=iris_cluster$centers[3,3],y=iris_cluster$centers[3,4]),pch=24,size=3,fill="black")+
  theme(legend.position= "bottom")

options(repr.plot.width=8,repr.plot.height=4)
grid.arrange(c,d,ncol=2)
