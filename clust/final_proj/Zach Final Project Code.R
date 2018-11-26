set.seed(12345)
load("C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Clustering/Data/final_data.Rdata")
library(splines)
library(factoextra)
library(dplyr)

##########################
## Dr. Wheeler Provided ##
##########################
times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
##########################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
##########################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)
##########################
# CONVERT EVERTYING TO 'numbers'
##########################
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$EVER_SMOKE)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)
##########################
##########################

## 1) Perform a principal components analysis on columns 2 through 65.
## List the standard deviations for the first 5 components. 
PCA <- princomp(cdata[,2:65])
PCA$sdev[1:5]

## 2) Using all pca scores compute the optimal number of clusters using kmeans using both
## "wss" and the "silhouette" method. What is the optimal number of components using each 
## method. Why may this number be different?
fviz_nbclust(scale(PCA$scores), kmeans, method = "wss",k.max=50)
fviz_nbclust(scale(PCA$scores), kmeans, method = "silhouette",k.max=50)

## 3) Run the command "set.seed(12345)" and run a k-means clustering algorithm using the
## pca scores.
set.seed(12345)

## 3a) Compute the graph of mean spirometry for the 4 clusters (all 4 on one graph).

## 3b) Look at cluster 3. Plot the graph of this cluster and give the mean values (on
## the original scale) for columns 2-65. What makes this cluster different from
## the other clusters?  Describe this cluster so a physician can better understand
## important characteristics of these clusters. 

## 3c) Looking at clusters 1,2, and 4 which clusters has the largest lung capacity?
## which one has the least lung capacity? Describe these three groups in terms of
## the curves as well as the additional variables that are available in the data
## frame cdata. Provide figures with your descriptions.

## 4) NOW look at the data using MCLUST type 'set.seed(12345)': 

## 4a) Using mclustbic() and columns 10-20 of cdata (NOT the principal component values).
## estimate the optimal number of  cluster components using the BIC and only with
## modelNames='VVV' and G = 1:20. Show a graph of the estimate. Is this number different than
## the ones given above, why? (This will take a while).

## 4b) Now using G = 6 and modelNames='VVV' and the same columns, provide a graph of each cluster's 
## mean curve (USING ALL OF THE DATA COLUMNS). Put all plots on one graph.

## 4c) Using all of the data compare cluster 4 with cluster 3 from the kmeans() cluster what can you 
## say about the similarities between these two clusters, what are the differences? Which estimate
## makes more sense? What do you trust more? What are the benefits of using mixture modeling over
## kmeans, what are the issues?

## 4d) Are there any clusters similar to the k-means clusters? Describe each cluster.

