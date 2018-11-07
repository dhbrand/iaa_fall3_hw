#############################
#Manufacture a fake dataset
#
#############################

set.seed(12345)
v1<- matrix(c(0.20,0.13,0.13,0.20),nrow=2,ncol=2)
group1 <- t(t(chol(v1))%*%matrix(rnorm(200),nrow=2) + matrix(c(1,1),nrow=2,ncol=100))

v2<- matrix(c(2,-0.13,-0.13,2),nrow=2,ncol=2)
group2 <- t(t(chol(v2))%*%matrix(rnorm(200),nrow=2) + matrix(c(1,1),nrow=2,ncol=100))

id <- c(rep(1,100),rep(2,100))
g <-  rbind(group1,group2)

df <- data.frame(id = id, x = g[,1],y=g[,2])

###############################################
#plot what my dataset looks like 
#
###############################################
library(ggplot2)

ggplot(data=df,mapping=aes(x=x,y=y,color=id)) + geom_point()


###############################################
# Now we look at what K-Means does with this dataset
#
###############################################
a<-kmeans(df[,2:3],2)
df <- data.frame(id = a$cluster, x = g[,1],y=g[,2])
ggplot(data=df,mapping=aes(x=x,y=y,color=id)) + geom_point()


###################################
#Clustering with Gaussian Mixture Models
#
###################################
library(mclust)
clustBIC <-mclustBIC(df[,-1])   # This is model selection
plot(clustBIC)

#cluster based upon 2 model components
clust    <- Mclust(df[,-1],G=2) # G is the number of components
df$class <- as.factor(clust$classification)
df$id <- id

ggplot(data=df,mapping=aes(x=x,y=y,color=id,shape=class)) + geom_point()

#########################################
# Now you do this with the iris data
#
#########################################

head(iris)