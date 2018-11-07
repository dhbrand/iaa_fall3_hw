library(mclust)
library(readr)

winequality_red <- read_csv("winequality-red.csv")

clPairs(winequality_red[,1:5],winequality_red$quality)

clustBIC <-mclustBIC(winequality_red[,1:5])   # For all of the data
                                              # It will take a long time for this 
                                              # problem ~ approx 10 minutes
                                              # we are just looking at 5 columns
plot(clustBIC)
clust    <- Mclust(winequality_red[,1:5],G=7)
plot(clust) #one way to make our decisions

table(clust$classification,winequality_red$quality) #look at how the clusters
                                                    #break up the wine quality

kmean_7 <- kmeans(winequality_red[,1:5],7,nstart=20)
table(kmean_7$cluster,winequality_red$quality)