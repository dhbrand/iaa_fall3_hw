library(tidyverse)
library(tidytext)
library(SnowballC)
library(text2vec)

reviews <- read_csv("clust/data/reviews.csv")
listings <- read_csv("clust/data/listings.csv")
str(reviews)
str(listings)
View(listings)
View(reviews)

# check the number of listings in each set:
#   2829 listings have reviews and 756 don't -> we use all those with reviews
(n_ids <- n_distinct(reviews$listing_id)) # 2829
(n_ids <- n_distinct(listings$id)) # 3585
nrow(listings[listings$number_of_reviews ==0,]) # 756

#subset_id = listings %>% inner_join(rv, by=c("id"="listing_id")) 
View(reviews[reviews$n==1,])
# ------------------ 
reviews <- reviews %>% group_by(listing_id) %>%
  add_count(listing_id, sort = TRUE) 

new_reviews <- reviews %>%
  # using listing id as the document
  group_by(listing_id) %>%
  # getting the individual words from the comments
  unnest_tokens(word, comments)  %>%
  # removing stop words from the comments
  anti_join(stop_words) %>%
  # regex to get only words (ie not numbers or characters)
  mutate(word = str_extract(word, "[a-z']+")) %>%
  # getting the stem of each word using only words in the english language
  #mutate(word = wordStem(word, language = "english")) %>%
  # removing missing values
  filter(!is.na(word)) %>%
  # filter any hard coded na's
  filter(word != "NA") 

score = new_reviews %>%
  left_join(nrc_total,by="word") %>%
  filter(!is.na(score)) %>% 
  group_by(listing_id) %>%
  mutate(sscore = sum(score)) %>% 
  distinct(listing_id,sscore)

nwords <- new_reviews %>% group_by(listing_id) %>% count(listing_id) 

complete <- nwords %>% left_join(score,"listing_id") %>% mutate(avg = sscore/nnn)

complete$avg <- scale(complete$avg) #standardize the score
nrow(complete)
View(complete)

combined = listings %>%
  filter(number_of_reviews !=0) %>%
  select(id,zipcode,latitude,longitude,property_type,room_type,price,
         availability_30,availability_60,availability_90,availability_365,
         number_of_reviews,review_scores_rating,review_scores_accuracy,
         review_scores_cleanliness,review_scores_checkin,review_scores_communication,
         review_scores_location,review_scores_value) %>%
  left_join(complete,by=c("id"="listing_id")) %>%
  rename(afinn_score=avg)
#table(combined$review_scores_rating)

# check for distributions
hist(combined$review_scores_rating, breaks=50, main='Review score rating Distribution', xlab='scores')
hist(combined$afinn_score, breaks=50, main='Afinn Sentiment Distribution', xlab='scores')
hist(combined$latitude, breaks=50, main='Latitude Distribution', xlab='scores')
hist(combined$longitude, breaks=50, main='Longitude Distribution', xlab='scores')

# clustering with geo locations and sentiment scores
toC = combined %>% 
  mutate(lon=scale(longitude),lat=scale(latitude)) %>%
  select(afinn_score, lon,lat)

clusters.c <- hclust(dist(toC),method="complete")
clusters.s <- hclust(dist(toC), method="single")
clusters.s <- hclust(dist(toC), method="average")

plot(clusters.c)
combined$clus1 <- cutree(clusters.c,6)

library(ggmap)
clu1 <- combined %>% filter(clus1 == 1)
clu2 <- combined %>% filter(clus1 == 2)
clu3 <- combined %>% filter(clus1 == 3)
clu4 <- combined %>% filter(clus1 == 4)
clu5 <- combined %>% filter(clus1 == 5)
clu6 <- combined %>% filter(clus1 == 6)

################################
# plot two interesting clusters
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu4, aes(x = longitude, y = latitude), color = 'red', size = 2)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu5, aes(x = longitude, y = latitude), color = 'red', size = 2)

###############################
# # creating varible for count - n
  # count(word, sort = TRUE) %>%
  # # finding the proportion of each word
  # mutate(p = n / sum(n)) %>%
  # # remove words with very little frequency
  # filter(p > 0.001) %>%
  # ungroup()

View(new_reviews)
View(new_reviews[new_reviews$listing_id==13225005,])
nrow(new_reviews)

nrc_total <- get_sentiments("afinn")


#####################################################
#find the sentiments that are positive and negative
nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")
######################################################

reviews_negative <-  new_reviews %>%
  group_by(listing_id) %>%
  inner_join(nrc_negative) %>% # join and count the negative words from the 
  count(word, sort = TRUE) %>% # review DB
  arrange(desc(listing_id)) 

reviews_positive <-  new_reviews %>%
  group_by(listing_id) %>%
  inner_join(nrc_positive) %>% # join and count the positive words from the 
  count(word, sort = TRUE) %>% # review DB
  arrange(desc(listing_id)) 

View(reviews_negative)

#look at all the unique positive and negative words that are in a review
pos_words <- sort(unique(reviews_positive$word))
neg_words <- sort(unique(reviews_negative$word))

########################################################
negative <- data.frame(matrix(0,nrow=length(unique(new_reviews$listing_id)),ncol = length(unique(reviews_negative$word))))
positive <- data.frame(matrix(0,nrow=length(unique(new_reviews$listing_id)),ncol = length(unique(reviews_positive$word))))

# So we know what each column means we name the column by the word it represents
names(negative) <- neg_words
names(positive) <- pos_words

#########################################################

#create my bag of words matrices
lsid <- unique(new_reviews$listing_id)
# asign the number of positive words found
for (ii in 1:nrow(reviews_positive)){
  x <- which(pos_words == reviews_positive$word[ii])
  y <- which(lsid      == reviews_positive$listing_id[ii])
  positive[y,x] = reviews_positive$nn[ii]
}
View(positive)
# asign the number of negative words found
for (ii in 1:nrow(reviews_negative)){
  x <- which(neg_words == reviews_negative$word[ii])
  y <- which(lsid      == reviews_negative$listing_id[ii])
  negative[y,x] = reviews_negative$nn[ii]
}

#######################################################
