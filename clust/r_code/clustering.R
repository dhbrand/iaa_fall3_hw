####################################################
# File: Clustering.R
# Purpose: Exploritory analysis of hierarchical 
# clustering methods in R
#
####################################################
library(datasets)
library(mlbench)
library(distances)

library(ggplot2)
head(iris)

##########################################
# Example 1: The choice of Linkage
###########################################
iris.clusters.single <- hclust(dist(iris[, 3:4]),method="single")
iris.clusters.complete <- hclust(dist(iris[, 3:4]),method="complete")
iris.clusters.centroid <- hclust(dist(iris[, 3:4]),method="average")

plot(iris.clusters.single,main="Single Linkage",xlab="")
plot(iris.clusters.complete,main="Complete Linkage",xlab="")
plot(iris.clusters.centroid,main="Average Linkage",xlab="")

#############################################
# Example 2: How good are our clusters
#            Complete linkage 
#############################################
iris.clusters <- hclust(dist(iris[, 1:2]))
plot(iris.clusters,xlab = "Cluster")

cutree <- cutree(iris.clusters,3)

iris.clusters <- hclust(dist(iris[, 1:2]))
clusters.s <- as.factor(cutree(iris.clusters,3))

iris.clusters <- hclust(dist(iris[, 3:4]))
clusters.p    <-  as.factor(cutree(iris.clusters,3))

iris.clusters <- hclust(dist(iris[, 3:4]),method="average")
clusters.p.a    <-  as.factor(cutree(iris.clusters,3))

iris.clusters <- hclust(dist(iris[1:4]))
clusters.a    <-  as.factor(cutree(iris.clusters,3))


# Set up the GGPLOT dataframe
plot.data <- data.frame(clusters.p = clusters.p,
                        clusters.s = clusters.s, 
                        clusters.a = clusters.a,
                        clusters.p.a = clusters.p.a,
                        P.Length = iris$Petal.Length,
                        P.Width  = iris$Petal.Width,
                        S.Length = iris$Sepal.Length,
                        S.Width  = iris$Sepal.Width,
                        species  = iris$Species)
###################################################################
#Look at what each does in relation to reality -complete linkage
ggplot(plot.data,aes(S.Length,S.Width,color=clusters.s,shape=species)) + geom_point() +
  scale_shape_manual(values = c(0, 16, 3))

ggplot(plot.data,aes(P.Length,P.Width,color=clusters.p,shape=species)) + geom_point() +
  scale_shape_manual(values = c(0, 16, 3))

ggplot(plot.data,aes(P.Length,P.Width,color=clusters.a,shape=species)) + geom_point() +
  scale_shape_manual(values = c(0, 16, 3))
#####################################################################
# average linkage
####################################################################
ggplot(plot.data,aes(P.Length,P.Width,color=clusters.p.a,shape=species)) + geom_point() +
  scale_shape_manual(values = c(0, 16, 3))

####################################################################
# Visual Qualitative grouping of the clusters 
####################################################################
plot(iris.clusters)
ans <- identify(iris.clusters)

####################################################################
# Example 3:
# Quantitative grouping of clusters
#
#
###################################################################
library(factoextra)

#create a function to 
#Need to define function
complete_Clust<-function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
average_Clust<-function(x,k){return(hcut(x,k, hc_method ="average" , hc_metric="euclidian"))}

fviz_nbclust(as.matrix(iris[,-5]), complete_Clust, method = "silhouette")
fviz_nbclust(as.matrix(iris[,-5]), average_Clust, method = "silhouette")

library(pvclust)

pvclust.fit <- pvclust(as.data.frame(t(iris[,-5])),method.hclust="complete",
                                  method.dist="euclidian",nboot=1000)
plot(pvclust.fit)
pvrect(pvclust.fit,0.95)

#Visualizing
library(factoextra)

complete_HCcluster_iris <- hcut(iris[, -5],6,hc_method ="single" , hc_metric="euclidian")

fviz_cluster(complete_HCcluster_iris, data = iris[, -5])

data(BostonHousing)
head(BostonHousing)

boston.clus <- hclust(dist(BostonHousing))

data(Glass)

glass.clus  <- hclust(dist(Glass[,1:9]))
plot(glass.clus)


glass.clus.m  <- hclust(dist(Glass[,1:9],method = "canberra"))
plot(glass.clus.m)
clusters.g <- cutree(glass.clus.m,6)
cbind(clusters.g,Glass[,10])

#compute mahalanobis distance matrix
my.cov <- cov(Glass[,1:9])
manhal.std <- as.matrix(Glass[,1:9],ncol=9)%*%t(chol(solve(my.cov)))
glass.manhal <- hclust(dist(manhal.std))
clusters.g <- cutree(glass.manhal,7)
cbind(clusters.g,Glass[,10])
mahalanobis(Glass[,1:9], Glass[,1:9], cov(Glass[,1:9]))


######################################
k.means <- kmeans(Glass[,1:9],6)
k.means <- kmeans(manhal.std,6)

