##################################################################
#Principle Component Analysis and clustering
#
#
##################################################################
library(readr)
Absenteeism_at_work <- read_delim("Absenteeism_at_work.csv", 
                                  ",", escape_double = FALSE, trim_ws = TRUE)
View(Absenteeism_at_work)

############################################
# First use principal component analysis to 
# find the components of variation
absent <- princomp(Absenteeism_at_work[,6:21])
absent$loadings
absent$center
absent$scores[1,]
##################################################################
# just a reminder how to build the scores on your own
temp <- Absenteeism_at_work[,6:21] - matrix(absent$center,ncol=16,nrow=740,byrow =TRUE)
temp <- as.matrix(temp)%*%absent$loadings
temp[1,]
absent$scores[1,]
##################################################################
#
#
library(factoextra)
library(dplyr)
# Plot this 
fviz_nbclust(scale(absent$scores), kmeans, method = "wss",k.max=20)
fviz_nbclust(scale(absent$scores), kmeans, method = "gap",k.max=20)
fviz_nbclust(scale(absent$scores), kmeans, method = "silhouette",k.max=20)

##################################################################
means_absent <- colMeans(absent$scores)
absent$center
# so we are scaling the scores but not modifying their meen ->
# important to know when we use it to plot
###################################################################
#compare the SD between the princ component analysis
sd_absent    <- apply(absent$scores,2,sd)
sd_absent 
absent$sdev
###################################################################
#There are about 17 clusters!?!
###################################################################
kmean_17 <- kmeans(scale(absent$scores),17,nstart=25)
Absenteeism_at_work$clust <- kmean_17$cluster
###################################################################
# Try to understand the clusters
###################################################################
#
clusters <- list()
for( ii in 1:17){
  clusters[[ii]] <-  Absenteeism_at_work %>% filter(clust == ii)
}
#
####################################################################

# Find the means of each cluster to "Name them"
x <- cbind(colMeans(Absenteeism_at_work))
y <- x
for (ii in 1:17) {
  x <- cbind(x,colMeans(clusters[[ii]])-y)
}

######################################################################
# Plot some of the "centers"
######################################################################
plot(kmean_17$centers[,1],-1*kmean_17$centers[,2],ylab="Distance from Work",xlab = "Work Load",
              axes=F,xlim=c(-2.5,2.5),ylim=c(-1,1),pch = 16,col="Light Blue",cex=2)

abline(v=0,lty=2)
abline(h=0,lty=2)
# Why did I multiply the second one by -1? 

######################################################################
# Plot some of the "centers"
######################################################################
plot(kmean_17$centers[,3],kmean_17$centers[,2],ylab="Tall but close absentee",xlab = "Heavy",
     axes=F,xlim=c(-2.5,2.5),ylim=c(-1,1),pch = 16,col="Light Blue",cex=2)

abline(v=0,lty=2)
abline(h=0,lty=2)
# Why did I multiply the second one by -1? 