## ----------------------------------------------------------------------------
## Top ####
library(ggmap)
library(tidytext)
library(tidyverse)
library(magrittr)
library(text2vec)
library(Matrix)
library(tm)
library(SnowballC)
library(wordcloud)
## ----------------------------------------------------------------------------
# to register my api key
source("clust/proj1/ggmap_api.R")

## ----------------------------------------------------------------------------
# listings has information like rooms and rent
# reviews has information about the comments from renters

listings <- read_csv("clust/data/listings.csv")
reviews <- read_csv("clust/data/reviews.csv")

## ----------------------------------------------------------------------------
# we want to only get listings that have at least 4 reviews
rev_4_plus <- reviews %>%
  group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>%
  filter(n >= 4) %>%
  select(-"n") %>% 
  pull

# to keep on the rows from listings that we are using in analysis
listing_sub <- listings %>% 
  filter(id %in% rev_4_plus)

## ----------------------------------------------------------------------------
## Geocoding and Map ####
# retrieves the lat and lon of each listing
strtAddress <- listing_sub$street
lon<- matrix(0,nrow=length(strtAddress))
lat<- matrix(0,nrow=length(strtAddress))
for (ii in 1:length(strtAddress)){
  latLon <- geocode(strtAddress[ii],output="latlon")
  lon[ii] <- as.numeric(latLon[1])
  lat[ii] <- as.numeric(latLon[2])
}

# adding the  lat and lon back to the subsetted data
listing_k <-data.frame(listing_sub, lat = lat, lon = lon)

## ----------------------------------------------------------------------------
# retriving a map of boston that we can plot our segments on later
boston <- get_map(location = "Boston", zoom = 12)


## ----------------------------------------------------------------------------
## Sentiment Clustering ####
# break up the dataset's listing into individual words
# join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
new_reviews <- reviews %>% 
  # subset to reviews within the subset
  filter(listing_id %in% rev_4_plus) %>% 
  # get individual words from the comments
  unnest_tokens(word, comments) %>% 
  # get rid of any NA's
  filter(!is.na(word)) %>%
  # join with the afinnity lexicon library for sentiment analysis
  left_join(get_sentiments("afinn"), by = "word") %>%
  filter(!is.na(score)) %>% 
  ungroup


## ----------------------------------------------------------------------------
# Find the number of words scored
score <- new_reviews %>% 
  group_by(listing_id) %>% 
  mutate(sscore = sum(score)) %>% 
  distinct(listing_id, sscore)

nwords <- new_reviews %>% 
  group_by(listing_id) %>% 
  count(listing_id)

complete <- nwords %>% 
  left_join(score, "listing_id") %>% 
  mutate(avg = sscore / n) %>% 
  arrange(listing_id)

## ----------------------------------------------------------------------------
# join with lat and lon from dave_geocode
combined <- complete %>% 
  left_join(listing_k)

## ----------------------------------------------------------------------------
# standardize score, lat, and lon for the clusterin algorithm
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)
complete$avg <- scale(complete$avg) 

# cbind automatically chooses matri for all numeric columns
scaled_mat <- cbind(combined$avg, combined$std.lat, combined$std.lon)

## ----------------------------------------------------------------------------
# running
clusters.c <- hclust(dist(scaled_mat), method = "complete")
clusters.s <- hclust(dist(scaled_mat), method = "single")
clusters.s <- hclust(dist(scaled_mat), method = "average")

plot(clusters.c)
combined$clus <- cutree(clusters.c, 6) # it looks like 6 clusters is reasonable

clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)

## ----------------------------------------------------------------------------
# plot two interesting clusters
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu4, aes(x = lon, y = lat), color = "red", size = 2)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu5, aes(x = lon, y = lat), color = "red", size = 2)

## ----------------------------------------------------------------------------
words4 <- new_reviews %>%
  right_join(clu4, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)


words5 <- new_reviews %>%
  right_join(clu5, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)


## ----------------------------------------------------------------------------
# building wordclouds to look at unique words which define cluster memberships
set.seed(555)
wordcloud(
  words = words4$word, freq = words4$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

set.seed(555)
wordcloud(
  words = words5$word, freq = words5$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

## ----------------------------------------------------------------------------
#  building clusters near popular attractions
#  first were getting the lat and lon of some attractions across the city
attractions <- tibble( 
  'place' = c('bunker hill monument', 'pier park sailing center', 'the paul revere house', 'new england aquarium', 'fenway park', 'boston convention and exhibition center', 'boston common', 'mit'),
  'lat' = c(42.37642693710535, 42.36494941392239, 42.363744469413895, 42.35914644250843, 42.3466825122026, 42.34535034292539, 42.35496035991529, 42.36009778602672),
  'lon' =  c(-71.06077194213867, -71.03626728057861, -71.05381965637207, -71.04976415634155, -71.09724998474121, -71.04643821716309, -71.06549263000488, -71.09416007995605)
)

## ----------------------------------------------------------------------------
## converting matrix to tibble and then dropping NA's for kmean algo
scaled_df <- scaled_mat %>% 
  as_tibble() %>% 
  drop_na()


## the centers of the clusters are the lat and lon of each attraction
k_clus <- kmeans(toC2[,2:3], centers = as.matrix(scale(attractions[,2:3])))

## adding the clusters back to the scaled data frame
clus_df <- scaled_df %>% 
  add_column(k_clus$cluster) %>% 
  set_colnames(c('avg', 'std_lat', 'std_lon', 'clus'))

## joing the clusters back to the original data but only keep listing that didn't have NA's 
clus_df_comb <- left_join(clus_df, combined, by = 'avg')

# plotting clusters around attractions
ggmap(map) +
  geom_point(data = clus_df_comb, aes(x = lon, y = lat, color = factor(clus.x)), size = 2) +
  theme(legend.position = 'None')



#####################################################
# find the sentiments that are positive and negative
nrc_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
######################################################

reviews_negative <- new_reviews %>%
  group_by(listing_id) %>%
  inner_join(nrc_negative) %>% # join and count the negative words from the
  count(word, sort = TRUE) %>% # review DB
  arrange(desc(listing_id))

reviews_positive <- new_reviews %>%
  group_by(listing_id) %>%
  inner_join(nrc_positive) %>% # join and count the positive words from the
  count(word, sort = TRUE) %>% # review DB
  arrange(desc(listing_id))

########################################################################
# look at all the unique positive and negative words that are in a review
pos_words <- sort(unique(reviews_positive$word))
neg_words <- sort(unique(reviews_negative$word))
#########################################################################
negative <- data.frame(matrix(0, nrow = length(unique(new_reviews$listing_id)), ncol = length(unique(reviews_negative$word))))
positive <- data.frame(matrix(0, nrow = length(unique(new_reviews$listing_id)), ncol = length(unique(reviews_positive$word))))

########################################################
# So we know what each column means we name the column by the word it represents
#########################################################
names(negative) <- neg_words
names(positive) <- pos_words

# create my bag of words matrices
lsid <- unique(new_reviews$listing_id)
# asign the number of positive words found
for (ii in 1:nrow(reviews_positive)) {
  x <- which(pos_words == reviews_positive$word[ii])
  y <- which(lsid == reviews_positive$listing_id[ii])
  positive[y, x] <- reviews_positive$n[ii]
}

# asign the number of negative words found
for (ii in 1:nrow(reviews_negative)) {
  x <- which(neg_words == reviews_negative$word[ii])
  y <- which(lsid == reviews_negative$listing_id[ii])
  negative[y, x] <- reviews_negative$n[ii]
}

#######################################################
# We have essentially created a "BAG" of words.
# Each line is the unique listing, and we have a count of the number
# of times a word is used, we have a negative list and a positive list
#######################################################

# COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
SP.COS <- 1 - sim2(Matrix(as.matrix(positive)), method = "cosine", norm = "l2")
SP.JAC <- 1 - sim2(Matrix(as.matrix(positive)), method = "jaccard")

B <- SP.COS
## Need to do this because R hclust is stupid
# it needs to know the number of observations
attr(B, "Size") <- as.integer(nrow(B)) 

C <- SP.JAC
attr(C, "Size") <- as.integer(nrow(C)) 


POSITIVE_CLUSTERS_COS <- hclust(as.dist(B)) # CLUSTER as.dist(B) convert it to a 'distance'
POSITIVE_CLUSTERS_JAC <- hclust(as.dist(C)) # because again hclust is stupid

#####################################################
# Let's just pick 10 clusters and see what happens
# When we build a word cloud
#####################################################
POSITIVE_GROUPS <- cutree(POSITIVE_CLUSTERS_COS, 10)
listing_group <- as.data.frame(cbind(POSITIVE_GROUPS, lsid))
names(listing_group) <- c("group", "listing_id")

words_and_clusters <- reviews_positive %>% right_join(listing_group, "listing_id")


for (ii in 1:10) {
  temp <- words_and_clusters %>%
    group_by("word") %>%
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(
    words = temp$word, freq = temp$nn, min.freq = 500,
    max.words = 200, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}



#######################################################
# Let's look at the negative words!!
#
#
#######################################################

# COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
SP.COS <- 1 - sim2(Matrix(as.matrix(negative)), method = "cosine", norm = "l2")
SP.JAC <- 1 - sim2(Matrix(as.matrix(negative)), method = "jaccard")

B <- SP.COS
attr(B, "Size") <- as.integer(nrow(B)) ## Need to do this because R hclust is stupid
# it needs to know the number of observations

C <- SP.JAC
attr(C, "Size") <- as.integer(nrow(C)) ## Need to do this because R hclust is stupid
# it needs to know the number of observations


NEGATIVE_CLUSTERS_COS <- hclust(as.dist(B), method = "single") # CLUSTER as.dist(B) convert it to a 'distance'
NEGATIVE_CLUSTERS_JAC <- hclust(as.dist(C), method = "single") # ecause again hclust is stupid

#####################################################
# Let's just pick 10 clusters and see what happens
# When we build a word cloud
#####################################################
NEGATIVE_GROUPS <- cutree(NEGATIVE_CLUSTERS_COS, 10)
listing_group <- as.data.frame(cbind(NEGATIVE_GROUPS, lsid))
names(listing_group) <- c("group", "listing_id")

words_and_clusters <- reviews_negative %>% right_join(listing_group, "listing_id")


for (ii in 1:10) {
  temp <- words_and_clusters %>%
    group_by("word") %>%
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(
    words = temp$word, freq = temp$nn, min.freq = 1,
    max.words = 100, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}

##############################
#
##############################
# narrow it down to words that were uses 10 or more times
temp_negative <- negative[, idx > 300]
# temp_negative <- temp_negative[,c(1,4,9,13)]
library(tidyverse)
library(knitr)
library(factoextra)
temp_negative <- scale(temp_negative)
SP.COS <- 1 - sim2(Matrix(as.matrix(temp_negative)), method = "cosine", norm = "l2")
SP.JAC <- 1 - sim2(Matrix(as.matrix(temp_negative)), method = "jaccard")

B <- SP.COS
attr(B, "Size") <- as.integer(nrow(B)) ## Need to do this because R hclust is stupid
## it needs to know the number of observations

C <- SP.JAC
attr(C, "Size") <- as.integer(nrow(C)) ## Need to do this because R hclust is stupid
## it needs to know the number of observations


NEGATIVE_CLUSTERS_COS <- hclust(as.dist(B), method = "average") # CLUSTER as.dist(B) convert it to a 'distance'
# NEGATIVE_CLUSTERS_JAC  <- hclust(as.dist(C),method="average")#ecause again hclust is stupid


NEGATIVE_GROUPS <- cutree(NEGATIVE_CLUSTERS_COS, 15)
listing_group <- as.data.frame(cbind(NEGATIVE_GROUPS, lsid))
names(listing_group) <- c("group", "listing_id")

words_and_clusters <- reviews_negative %>% right_join(listing_group, "listing_id")

for (ii in 1:15) {
  temp <- words_and_clusters %>%
    group_by("word") %>%
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
  par(mar = rep(0, 4))
  plot.new()
  text(x = 0.5, y = 0.5, sprintf("Group #%d", ii))
  wordcloud(
    words = temp$word, freq = temp$nn, min.freq = 1,
    max.words = 100, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}

################################################
## CLUSTER 5 IS INTERESTING "DIE" IS THE MOST USED TERM ...
############################################

temp <- words_and_clusters %>%
  filter(group == 5) %>%
  count(word, sort = TRUE)


# finding the optimal number of clusters to cut
complete_Clust<-function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
average_Clust<-function(x,k){return(hcut(x,k, hc_method ="average" , hc_metric="euclidian"))}

fviz_nbclust(as.matrix(B), complete_Clust, method = "silhouette")
fviz_nbclust(as.matrix(B), average_Clust, method = "silhouette")




