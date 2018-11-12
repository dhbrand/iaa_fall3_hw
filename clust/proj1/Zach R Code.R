
###############################################
library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
###############################################

nrc_total <- get_sentiments("afinn")

#Load the dataset and group the 
#reviews by the listing id 
#keep all listings having more than 4 reviews 
##reviews <- read_csv("C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Clustering/Data/boston-airbnb-open-data/reviews.csv")
reviews <- read_csv("clust/data/reviews.csv")

rv <- reviews %>% group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>% filter( n >= 4) %>% select(-"n")

#break up the dataset's listing into individual words
#join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
new_reviews <- reviews %>%
  group_by(listing_id) %>%
  unnest_tokens(word, comments)  %>%
  right_join(rv,by="listing_id") %>% filter(!is.na(word)) %>%
  left_join(nrc_total,by="word") %>% filter(!is.na(score))

#Find the number of words scored
score         <- new_reviews %>% group_by(listing_id) %>% mutate(sscore = sum(score)) %>% distinct(listing_id,sscore)
nwords        <- new_reviews %>% group_by(listing_id) %>% count(listing_id) 

complete <- nwords %>% left_join(score,"listing_id") %>% mutate(avg = sscore/n)

complete$avg <- scale(complete$avg) #standardize the score
hist(complete$avg)

##listing_k = read_csv("C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Clustering/data/boston-airbnb-open-data/listings.csv")
listing_k <- read_csv("clust/data/listings.csv")
listing_k$listing_id = listing_k$id
combined <- complete %>% left_join(listing_k,"listing_id")
combined$std.lat <- scale(combined$latitude)
combined$std.lon <- scale(combined$longitude)

toC<- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toC),method="complete")
clusters.s <- hclust(dist(toC), method="single")
clusters.s <- hclust(dist(toC), method="average")

plot(clusters.c)
combined$clus <- cutree(clusters.c,4) #it looks like 4 clusters is reasonable

#install.packages("ggmap")
library(ggmap)
clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)

################################
# plot two interesting clusters
##map <- readr::read_rds("C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Clustering/Data/boston.rds")
map <- readr::read_rds("clust/data/boston.rds")

ggmap(map, extent = TRUE) +
  geom_point(data = clu1, aes(x = longitude, y = latitude), color = 'red', size = 2)

ggmap(map, extent = TRUE) +
  geom_point(data = clu2, aes(x = longitude, y = latitude), color = 'red', size = 2)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu3, aes(x = longitude, y = latitude), color = 'red', size = 2)

ggmap(map, extent = TRUE) +
  geom_point(data = clu4, aes(x = longitude, y = latitude), color = 'red', size = 2)


###############################
# describe them
###############################

# did as.data.frame because new_reviews
# was made to be a tibble with listing_id being a 'group'
# as.data.frame removes this distinction

words1 <- as.data.frame(new_reviews) %>% right_join(clu1,'listing_id') %>%
  select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)
words2 <- as.data.frame(new_reviews) %>% right_join(clu2,'listing_id') %>%
  select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)
words3 <- as.data.frame(new_reviews) %>% right_join(clu3,'listing_id') %>%
  select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)
words4 <- as.data.frame(new_reviews) %>% right_join(clu4,'listing_id') %>%
  select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)


#random is not so random
set.seed(555)

library(wordcloud)
wordcloud(words = words1$word, freq = words1$n, min.freq = 150,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(555)
wordcloud(words = words5$word, freq = words5$n, min.freq = 150,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#create summary stats for each cluster based on combined dataset
cluster_stats <- function(dataframe){
  unique_clusters = unique(dataframe$clus)
  temp = NULL
  for(i in unique_clusters){
    cluster_i = dataframe[which(dataframe$clus==i), 
                          c('sscore', 'clus', 'price', 'beds', 'review_scores_rating', 
                            'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                            'review_scores_communication', 'review_scores_location', 'review_scores_value')]
    cluster_i$price_new = as.numeric(gsub("\\$", "", cluster_i$price))
    cluster_i$beds[cluster_i$beds == 0] = NA
    cluster_i$price_per_bed = cluster_i$price_new/cluster_i$beds
    
    avg_score = mean(cluster_i$sscore, na.rm=TRUE)
    avg_price_per_bed = mean(cluster_i$price_per_bed, na.rm=TRUE)
    avg_beds = mean(cluster_i$beds, na.rm=TRUE)
    avg_price = mean(cluster_i$price_new, na.rm=TRUE)
    avg_rating = mean(cluster_i$review_scores_rating, na.rm=TRUE)
    avg_accuracy = mean(cluster_i$review_scores_accuracy, na.rm=TRUE)
    avg_cleanliness = mean(cluster_i$review_scores_cleanliness, na.rm=TRUE)
    avg_checkin = mean(cluster_i$review_scores_checkin, na.rm=TRUE)
    avg_communication = mean(cluster_i$review_scores_communication, na.rm=TRUE)
    avg_location = mean(cluster_i$review_scores_location, na.rm=TRUE)
    avg_value = mean(cluster_i$review_scores_value, na.rm=TRUE)
    

    temp = rbind(temp, data.frame(cluster = i, 
                                  "Average_Score" = avg_score, 
                                  "Average_Price" = avg_price, 
                                  "Average_Beds"  = avg_beds, 
                                  "Average_Price_per_Bed" = avg_price_per_bed,
                                  "Average_Rating" = avg_rating,
                                  "Average_Accuracy" = avg_accuracy,
                                  "Average_Cleanliness" = avg_cleanliness,
                                  "Average_Checkin" = avg_checkin,
                                  "Average_Communication" = avg_communication,
                                  "Average_Location" = avg_location,
                                  "Average_Value" = avg_value))
  }
  cluster_stats <<- temp
}
cluster_stats(combined)
cluster_stats
