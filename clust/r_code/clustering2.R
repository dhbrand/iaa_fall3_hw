set.seed(8673209)

#create three clusers, using multivariate normal distribution
cov1 <- matrix(c(0.3,0.25,0.25,0.3),nrow=2); m1 <- matrix(c(1,3.5),nrow=2);
cov2 <- matrix(c(0.25,-0.15,-0.15,0.25),nrow=2); m2 <- matrix(c(-3,3),nrow=2);
cov3 <- matrix(c(0.25,0,0,0.25),nrow=2); m3 <- matrix(c(-0.5,1),nrow=2) ;

a1 <- t(chol(cov1))%*%matrix(rnorm(200),ncol=100,nrow=2) + matrix(m1,ncol=100,nrow=2)
a2 <- t(chol(cov2))%*%matrix(rnorm(200),ncol=100,nrow=2) + matrix(m2,ncol=100,nrow=2)
a3 <-t(chol(cov3))%*%matrix(rnorm(200),ncol=100,nrow=2) + matrix(m3,ncol=100,nrow=2)

group <- rep(c(1,2,3),each=100)

#plot those cluseters
data <- rbind(t(a1),t(a2),t(a3))
plot(data,xlab="",ylab="",axes=FALSE)
points(t(a1),col=1,pch=16,cex=1.2)
points(t(a2),col=2,pch=16,cex=1.2)
points(t(a3),col=3,pch=16,cex=1.2)

clusters      <- hclust(dist(data))
plot(clusters)
myclusters   <-  cutree(clusters,3)

#Calculate the within cluster sum of squares
#manually
for (jj in 1:10){
 myclusters   <-  cutree(clusters,jj)

WSS <- 0
for (ii in unique(myclusters)){
  temp <- colMeans(data[myclusters==ii,]) #find each column mean
  temp.M <- as.matrix(dist(rbind(data[myclusters==ii,],temp))) # find the distance from 
                                                              #  the mean 
  WSS[ii] <-sum(temp.M[nrow(temp.M),]^2) #square the distance from the mean 
}

cat(sum(WSS),"\n")
}

## The easy automatic way that makes 
## pretty pictures

####################################################
# WSS Statistic
####################################################
library(factoextra)
#create a function to 
#Need to define function
complete_Clust<-function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
fviz_nbclust(as.matrix(data), complete_Clust, method = "wss")

library(factoextra)

###################################################
# Gap Statistic
####################################################

max.B <- apply(data,2,max)
min.B <- apply(data,2,min)

l <- nrow(data)
####################################
#Null distribution data
####################################
ndata <- cbind(runif(l,min.B[1],max.B[1]),runif(l,min.B[2],max.B[2]))
clusters      <- hclust(dist(ndata))
myclusters   <-  cutree(clusters,3)

plot(ndata,xlab="",ylab="",axes=FALSE)
points(ndata[myclusters==1,],col=1,pch=16,cex=1.2)
points(ndata[myclusters==2,],col=2,pch=16,cex=1.2)
points(ndata[myclusters==3,],col=3,pch=16,cex=1.2)

###################################################
# Silhouette Statistic
####################################################
library(factoextra)
#create a function to 
#Need to define function
complete_Clust<-function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
fviz_nbclust(as.matrix(data), complete_Clust, method = "silhouette")

#create a function to 
#Need to define function
complete_Clust<-function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
fviz_nbclust(as.matrix(data), complete_Clust, method = "wss")
fviz_nbclust(as.matrix(data), complete_Clust, method = "gap_stat")


###################################################
# Look at Iris data: 
###################################################

library(factoextra)

#create a function to 
#Need to define function
complete_Clust<-function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
fviz_nbclust(as.matrix(iris[,-5]), complete_Clust, method = "wss")
fviz_nbclust(as.matrix(iris[,-5]), complete_Clust, method = "gap_stat")
fviz_nbclust(as.matrix(iris[,-5]), complete_Clust, method = "silhouette")