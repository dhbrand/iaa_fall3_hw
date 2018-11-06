library(tidyverse)
library(knitr)
library(factoextra)
## Get rows from the summary

library(readr)
sotc_08 <- load("clust/data/community survey.RData")

communitySum <- sotc_08[,c(136,138,140,144:154)]
x <-cbind(communitySum,1:nrow(communitySum)) 
df <- na.omit(x)
id <- df[,15]
df <- scale(df[,-15]) #do this to make everthing mean = 0, sd = 1 

set.seed(8675309)
reduced.df <- sample_frac(as.data.frame(df),size = 0.10) # randomly sample 10% of the data
                                                         # full dataset is too large

fviz_nbclust(reduced.df, kmeans, method = "wss")
fviz_nbclust(reduced.df, kmeans, method = "gap")
fviz_nbclust(reduced.df, kmeans, method = "silhouette")

kmeans_2 <- kmeans(df,2,nstart=25)
kmeans_3 <- kmeans(df,3,nstart=25)

kmeans_2$centers
kmeans_3$centers

############################################
#
#Let's pick 3 clusters and go from there.. 
############################################
data.table <- data.frame(clust = kmeans_3$cluster, wage = sotc_08$QD9[id], race = sotc_08$QD111[id], 
                         edu = sotc_08$QD7[id])

## Look at the proportion of the sample 
## Falling into a specific cluster
data.table %>%
              group_by(clust)%>%
              summarize(n=n())%>%
              mutate(prop=n/sum(n)) %>%
              kable()


data.table %>%
          group_by(wage,clust)%>%
          summarize(n=n())%>%
          mutate(prop=n/sum(n)) %>%
          kable()

# Look at 

data.table %>%
          group_by(race,clust)%>%
          summarize(n=n())%>%
          mutate(prop=n/sum(n)) %>%
          kable()

data.table %>%
          group_by(edu,clust)%>%
          summarize(n=n())%>%
          mutate(prop=n/sum(n)) %>%
          kable()

data.table %>%
          group_by(clust)%>%
          summarize(n=n())%>%
          mutate(prop=n/sum(n)) %>%
          kable()

install.packages("gmodels")
library(gmodels)

CrossTable(data.table$edu,data.table$clust)

##########################################
## Look at the additional demographic info
## what else do you see? 
############################################
