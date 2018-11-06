

##############################################
#Diferent Way to look at it
###############################################
nrc_total <- get_sentiments("afinn")

#######################################################
library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)


##############################################
#Diferent Way to look at it
###############################################
nrc_total <- get_sentiments("afinn")

#Load the dataset and group the 
#reviews by the listing id 
#keep all listings having more than 4 reviews 
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

combined <- complete %>% left_join(listing_k,"listing_id")
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

toC<- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toC),method="complete")
clusters.s <- hclust(dist(toC), method="single")
clusters.s <- hclust(dist(toC), method="average")

plot(clusters.c)
combined$clus <- cutree(clusters.c,6) #it looks like 6 clusters is reasonable

library(ggmap)
clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)

################################
# plot two interesting clusters
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu4, aes(x = lon, y = lat), color = 'red', size = 2)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu5, aes(x = lon, y = lat), color = 'red', size = 2)

###############################
# describe them
###############################

# did as.data.frame because new_reviews
# was made to be a tibble with listing_id being a 'group'
# as.data.frame removes this distinction

words4 <- as.data.frame(new_reviews) %>% right_join(clu4,'listing_id') %>%
          select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)

 
words5 <- as.data.frame(new_reviews) %>% right_join(clu5,'listing_id')  %>%
          select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)

#random is not so random
set.seed(555)

wordcloud(words = words4$word, freq = words4$n, min.freq = 150,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(555)
wordcloud(words = words5$word, freq = words5$n, min.freq = 150,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

