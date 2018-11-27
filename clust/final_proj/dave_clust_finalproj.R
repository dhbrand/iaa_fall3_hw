library(splines)
library(mclust)
library(tidyverse)
library(factoextra)
library(knitr)

load('../fall_3_team_4/clust/data/final_data.Rdata')
times <- seq(1,295)/100 # Observations in 1/100th of a second 
X <- bs(times,intercept=TRUE,df=60) #create a spline to
#model the data 
betas <- matrix(0,ncol=60,nrow = 6792)
########################################################### 
# run a linear regression on each data set
# here I am manipulating my data you I can cluster 
########################################################### 
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,] <- coefficients(temp) 
  }
cdata <- cbind(final_data[,1:5],betas)
#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE) 
cdata$ASTHMA <- as.numeric(cdata$ASTHMA) 
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

############################################
# a) Perform a principal components analysis on columns 2 through 65. List the standard deviations for the first 5 components.
sub <- cdata[,2:65]
pca <- princomp(sub[,2:65])
print(pca$loadings, digits = 2, sort = TRUE)
pca$center

kable(summarize_all(as_tibble(pca$scores[,1:5]), funs(sd), na.rm=TRUE))
# |   Comp.1|   Comp.2|   Comp.3|   Comp.4|   Comp.5|
# |--------:|--------:|--------:|--------:|--------:|
# | 45.21508| 31.29238| 22.37703| 17.33123| 13.04808|
##################################################################
Xpca <- prcomp(cdata[,2:65])

mu = colMeans(cdata[,2:65])

nComp = 5
Xhat = Xpca$x[,1:nComp] %*% t(Xpca$rotation[,1:nComp])
Xhat = scale(Xhat, center = -mu, scale = FALSE)

Xhat[1, 1:4]


##################################################################
# b) Using all pca scores compute the optimal number of clusters using kmeans using both "wss" and the "silhouette" method. What is the optimal number of components using each method. Why may this number be different?
# Plot this 
fviz_nbclust(scale(pca$scores), kmeans, method = "wss", k.max = 100, iter.max = 100) # no optimal clusters
fviz_nbclust(scale(pca$scores), kmeans, method = "silhouette", k.max = 100, iter.max = 100) # 19 clusters


###################################################################
# c) Run the command "set.seed(12345)" and run a k-means clustering algorithm using the pca scores.
###################################################################
set.seed(12345)
kmean_4 <- kmeans(scale(pca$scores), 4, nstart = 25, iter.max=100, trace = 1)
sub$clust <- kmean_4$cluster
###################################################################
# Try to understand the clusters
###################################################################

clusters <- list()
for( ii in 1:4){
  clusters[[ii]] <-  sub %>% filter(clust == ii)
}
#
####################################################################
# Find the means of each cluster to "Name them"
x <- cbind(colMeans(sub[,-65]))
y <- x
for (ii in 1:4) {
  x <- cbind(x,colMeans(clusters[[ii]][,-65]))
}

n_clus <- cdata %>% 
  group_by(clust) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  pull(clust)

x[1:4,]
kable(x[2:5,])
  # |              |    avg    |   clus1   |   clus2   |  clus3   |   clus4   |
  # |:-------------|----------:|----------:|----------:|---------:|----------:|
  # |AGE           | 30.1303004| -0.6523462| -0.4260750| 0.7360481|  0.6968470|
  # |EVER_SMOKE    |  0.4561249| -0.4561249| -0.1392234| 0.0784814|  0.5438751|
  # |ASTHMA        |  0.0625736| -0.0625736| -0.0203201| 0.9374264| -0.0625736|
  # |POVERTY_RATIO |  2.2065340|  0.0936606| -0.1587382| 0.0185304| -0.1074078|

# Cluster 1 youngest never smoke no asthma higest poverty - 
# Cluster 2 2nd youngest light smoker very light asthma least poverty
# Cluster 3 oldest half smokers all all asthma 2nd higest poverty
# Cluster 4 2nd oldest all smokers no asthma 2nd least poverty

cent_df <- tibble(clus1 = kmean_4$centers[,1], 
                  clus2 = kmean_4$centers[,2], 
                  clus3 = kmean_4$centers[,3], 
                  clus4 = kmean_4$centers[,4]) %>% 
  gather(cluster, center)
######################################################################
# a) Compute the graph of mean spirometry for the 4 clusters (all 4 on one graph)
######################################################################
ggplot(cent_df, aes(factor(cluster), center)) +
  geom_violin()+
  theme_bw() + 
  theme(panel.border = element_blank()) +
  scale_x_continuous(limits = c(-.1, .1)) +
  scale_y_continuous(limits = c(-.75, .75)) +
  geom_point(aes(clus1, clus3, color = 'red'))



plot(kmean_4$centers[,1],-1*kmean_4$centers[,2],ylab="Distance from Work",xlab = "Work Load",
     axes=F,pch = 16,col="Light Blue",cex=2)

abline(v=0,lty=2)
abline(h=0,lty=2)
# Why did I multiply the second one by -1? 

######################################################################
# b) Look at cluster 3. Plot the graph of this cluster and give the mean the other clusters? Describe this cluster so a physician can better values (on different from understand capacity? the original scale) for columns 2-65. What makes this cluster important characteristics of these clusters.
######################################################################
clus3 <- sub %>% 
  filter(clust == 3) %>% 
  select(1:4) 

ggplot(clus3, aes(AGE)) +
  geom_histogram()

# Why did I multiply the second one by -1? 
