# top ---------------------------------------------------------------------
library(ggmap)
library(tidytext)
library(tidyverse)
library(magrittr)
library(text2vec)
library(Matrix)
library(tm)
library(SnowballC)
library(wordcloud)
library(geosphere)
library(FactoMineR)
library(factoextra)
library(mclust)
library(cluster)
# to register my api key
source("clust/proj1/ggmap_api.R")

# listings has information like rooms and rent
# reviews has information about the comments from renters

listings <- read_csv("clust/data/listings.csv")
reviews <- read_csv("clust/data/reviews.csv")


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


# geocoding and map -------------------------------------------------------
# retrieves the latitude and longitude of each listing
strtAddress <- listing_sub$street
longitude<- matrix(0,nrow=length(strtAddress))
latitude<- matrix(0,nrow=length(strtAddress))
for (ii in 1:length(strtAddress)){
  latitudeLon <- geocode(strtAddress[ii],output="latitudelon")
  longitude[ii] <- as.numeric(latitudeLon[1])
  latitude[ii] <- as.numeric(latitudeLon[2])
}

# adding the  latitude and longitude back to the subsetted data
listing_k <-data.frame(listing_sub, latitude = latitude, longitude = longitude)


# retriving a map of boston that we can plot our segments on latitudeer
boston <- get_map(location = "Boston", zoom = 12, color = 'bw')
#write_rds(boston, "clust/data/boston.rds")


# sentiment clusting w/ affinity lexicon ----------------------------------
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


# join with latitude and longitude from dave_geocode
combined <- complete %>% 
  left_join(listings, by = c('listing_id' = 'id')) %>% 
  ungroup


# standardize score, latitude, and longitude for the clusterin algorithm
combined$std_lat<- scale(combined$latitude)
combined$std_lon <- scale(combined$longitude)
combined$std_avg <- scale(complete$avg) 

# cbind automatically chooses matri for all numeric columns
scaled_mat <- cbind(combined$std_avg, combined$std_lat, combined$std_lon)


# using hierchical cluster to determine how many clusters we might use
clusters.c <- hclust(dist(scaled_mat), method = "complete")
clusters.s <- hclust(dist(scaled_mat), method = "single")
clusters.s <- hclust(dist(scaled_mat), method = "average")


# labels is false bc there are so many they jumble at the bottom
plot(clusters.c, labels = FALSE)
plot(clusters.a, labels = FALSE)
plot(clusters.s, labels = FALSE)

# it looks like 6 clusters is reasonable
combined$clus <- cutree(clusters.c, 6) 

clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)



ggmap(boston) +
  geom_point(data = clu2, aes(x = longitude, y = latitude), color = "red", size = 2)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu5, aes(x = longitude, y = latitude), color = "red", size = 2)



# hierarchical wordcloud clusters -----------------------------------------

words2 <- new_reviews %>%
  right_join(clu2, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)

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



# building wordclouds to look at unique words which define cluster memberships
set.seed(555)
wordcloud(
  words = words2$word, freq = words4$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

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


# attraction clustering w/ kmeans -----------------------------------------


#  building clusters near popular attractions
#  first were getting the latitude and longitude of some attractions across the city
attractions <- tibble( 
  'place' = c('bunker hill monument', 'pier park sailing center', 'the paul revere house', 'new england aquarium', 'fenway park', 'boston convention and exhibition center', 'boston common', 'mit'),
  'latitude' = c(42.37642693710535, 42.36494941392239, 42.363744469413895, 42.35914644250843, 42.3466825122026, 42.34535034292539, 42.35496035991529, 42.36009778602672),
  'longitude' =  c(-71.06077194213867, -71.03626728057861, -71.05381965637207, -71.04976415634155, -71.09724998474121, -71.04643821716309, -71.06549263000488, -71.09416007995605)
)


## converting matrix to tibble and then dropping NA's for kmean algo
scaled_df <- scaled_mat %>% 
  as_tibble() %>% 
  drop_na()


## the centers of the clusters are the latitude and longitude of each attraction
k_clus <- kmeans(scaled_mat[,2:3], centers = as.matrix(scale(attractions[,2:3])))

## adding the clusters back to the scaled data frame
clus_df <- scaled_df %>% 
  add_column(k_clus$cluster) %>% 
  set_colnames(c('avg', 'std_latitude', 'std_lon', 'clus'))

## joing the clusters back to the original data but only keep listing that didn't have NA's 
clus_df_comb <- left_join(clus_df, combined, by = 'avg')

# plotting clusters around attractions
ggmap(map) +
  geom_point(data = clus_df_comb, aes(x = longitude, y = latitude, color = factor(clus.x)), size = 2) +
  theme(legend.position = 'None')

# geo and sentiment clustering --------------------------------------------

# building kmeans clusters using attractions and random sample from sentiment scores
nrow(attractions)

# need 8 random values from avg
centroids <- cbind(sample(combined$std_avg, 8), scale(attractions[,2:3]))
k_clus2 <- kmeans(scaled_mat, centers = centroids)

## adding the clusters back to the scaled data frame
clus_df2 <- scaled_mat %>%
  as_tibble() %>% 
  add_column(k_clus2$cluster) %>% 
  set_colnames(c('std_avg', 'std_latitude', 'std_lon', 'clus'))

## joing the clusters back to the original data but only keep listing that didn't have NA's 
clus_df_comb2 <- left_join(clus_df2, combined, by = 'std_avg')

# plotting clusters around attractions
ggmap(boston) +
  geom_point(data = clus_df_comb2, aes(x = longitude, y = latitude, color = factor(clus)), size = 1.5)

# we still get what we hoped, clusters are geographic but now we have sentiment assocaited with them
clus_df_comb2 %>% 
  group_by(clus) %>% 
  summarise(avg_sent = mean(avg))


# geo sentiment clusters word clouds --------------------------------------
clu1 <- clus_df_comb2 %>% filter(clus == 1)
clu2 <- clus_df_comb2 %>% filter(clus == 2)
clu3 <- clus_df_comb2 %>% filter(clus == 3)
clu4 <- clus_df_comb2 %>% filter(clus == 4)
clu5 <- clus_df_comb2 %>% filter(clus == 5)
clu6 <- clus_df_comb2 %>% filter(clus == 6)
clu7 <- clus_df_comb2 %>% filter(clus == 7)
clu8 <- clus_df_comb2 %>% filter(clus == 8)

words2 <- new_reviews %>%
  right_join(clu2, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)

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



# building wordclouds to look at unique words which define cluster memberships
set.seed(555)
wordcloud(
  words = words2$word, freq = words4$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

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


# geo sentiment clust with pca --------------------------------------------
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

combined %<>% 
  # convert price from character to numeric
  modify_at("price", parse_number) %>% 
  # droppin NA's beca
  drop_na()


# get night rate by bed

combined %<>% 
  # convert price from character to numeric% 
  modify_at("price", parse_number) %>% 
  # need the price per night over number of beds
  mutate(
    bed_nt_rate = map2_dbl(price, beds, ~ .x/.y)
  ) %>% 
  filter(is.finite(.$bed_nt_rate)) %>% 
  # removing NA's from new var for scaling
  drop_na(bed_nt_rate)

# need to have columns for each attraction lat and lon the same length as combined dataframe to map to
for (l in seq_along(attractions$lat)) {
  assign(paste('attr',l,'lat', sep = '_'), 
         rep(attractions$lat[l], nrow(combined)))
}
for (l in seq_along(attractions$lon)) {
  assign(paste('attr',l,'lon', sep = '_'), 
         rep(attractions$lon[l], nrow(combined)))
}
attr_dist <- combined %>% 
  # selecting columns for clustering
  select(avg, bed_nt_rate, longitude, latitude) %>% 
  # creadting the new columns for listing distance to attractionss
  mutate(
    attr1_dist = pmap_dbl(list(longitude, latitude, attr_1_lon, attr_1_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr2_dist = pmap_dbl(list(longitude, latitude, attr_2_lon, attr_2_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr3_dist = pmap_dbl(list(longitude, latitude, attr_3_lon, attr_3_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr4_dist = pmap_dbl(list(longitude, latitude, attr_4_lon, attr_4_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr5_dist = pmap_dbl(list(longitude, latitude, attr_5_lon, attr_5_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr6_dist = pmap_dbl(list(longitude, latitude, attr_6_lon, attr_6_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr7_dist = pmap_dbl(list(longitude, latitude, attr_7_lon, attr_7_lat), ~ earth.dist(..1, ..2, ..3, ..4)),
    attr8_dist = pmap_dbl(list(longitude, latitude, attr_8_lon, attr_8_lat), ~ earth.dist(..1, ..2, ..3, ..4))
  ) %>% 
  as.matrix() %>% 
  scale()


# how correlated on the new distance variables
clPairs(attr_dist )

# as expected all the distances are highly correlated
# running PCA to get new features to remove the collineartiy 

dist_pca <- PCA(attr_dist, scale.unit = FALSE)
dist_pca
eig_val <- get_eigenvalue(dist_pca)
eig_val
fviz_eig(dist_pca, addlabels = TRUE)
# the first 5 principal components account for almost 99% variace

# looking at sq cosine to determine quality of represent for first 3 components, higher values are better
fviz_cos2(dist_pca, choice = 'var', axes = 1:5)
#  looks really good at capture all the attraction distances

# getting the scores for the first 5 components
princomp_var <- get_pca_ind(dist_pca)
princomp_1_5 <- princomp_var$coord[,1:5] 


# checking for correlated variables again
clPairs(princomp_1_5)
# finding the optimal number of clusters to cut
fviz_nbclust(as.matrix(dist(scaled_mat_final)), complete_Clust, method = "silhouette")
fviz_nbclust(as.matrix(dist(attr_dist)), average_Clust, method = "silhouette")

set.seed(123)
# Compute the gap statistic
gap_stat <- clusGap(princomp_1_5, FUN = kmeans, nstart = 25, iter.max = 20, 
                    K.max = 20, B = 100) 
# Plot the result
fviz_gap_stat(gap_stat)
# suggested 13 clusters

# Compute k-means
set.seed(123)
km.res <- kmeans(princomp_1_5, 13, nstart = 25)

# added clusters back to combined df
combined %<>% 
  add_column(clus = km.res$cluster)


# plotting clusters 
ggmap(boston) +
  geom_point(data = combined, aes(x = longitude, y = latitude, color = factor(clus)), size = 1.5)

# word clouds for some of the clusters

clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)
clu7 <- combined %>% filter(clus == 7)
clu8 <- combined %>% filter(clus == 8)
clu9 <- combined %>% filter(clus == 9)
clu10 <- combined %>% filter(clus == 10)
clu11 <- combined %>% filter(clus == 11)
clu12 <- combined %>% filter(clus == 12)
clu13 <- combined %>% filter(clus == 13)

words2 <- new_reviews %>%
  right_join(clu2, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)

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

words11 <- new_reviews %>%
  right_join(clu11, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)

words8 <- new_reviews %>%
  right_join(clu8, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)

# building wordclouds to look at unique words which define cluster memberships
set.seed(555)
wordcloud(
  words = words2$word, freq = words4$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

wordcloud(
  words = words5$word, freq = words5$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

wordcloud(
  words = words5$word, freq = words5$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

wordcloud(
  words = words11$word, freq = words11$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

wordcloud(
  words = words8$word, freq = words8$n, min.freq = 150,
  max.words = 100, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
# looking at the avg sentiment per cluster 

combined %>% 
  group_by(clus) %>% 
  summarise(avg_sent = mean(avg),
            avg_bed_rate = mean(bed_nt_rate), 
            avg_avail = mean(availability_365),
            n = n(), 
            freq = n/ nrow(combined), 
            avail_rate = avg_bed_rate*(365- avg_avail)
            ) %>% 
  arrange(avg_sent)

cluster_stats <- function(dataframe){
  unique_clusters = unique(dataframe$clus)
  temp = NULL
  for(i in unique_clusters){
    cluster_i = dataframe[which(dataframe$clus==i), 
                          c('avg', 'clus', 'price', 'beds', 'review_scores_rating', 
                            'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                            'review_scores_communication', 'review_scores_location', 'review_scores_value')]
    cluster_i$price_new = as.numeric(gsub("\\$", "", cluster_i$price))
    cluster_i$beds[cluster_i$beds == 0] = NA
    cluster_i$price_per_bed = cluster_i$price_new/cluster_i$beds
    
    avg_score = mean(cluster_i$avg, na.rm=TRUE)
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
View(cluster_stats)











# positive and negative sentiment analysis --------------------------------


# adding columns for the distance to each attraction from the listing

# find the sentiments that are positive and negative
nrc_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")


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


# look at all the unique positive and negative words that are in a review
pos_words <- sort(unique(reviews_positive$word))
neg_words <- sort(unique(reviews_negative$word))


negative <- data.frame(matrix(0, nrow = length(unique(new_reviews$listing_id)), ncol = length(unique(reviews_negative$word))))
positive <- data.frame(matrix(0, nrow = length(unique(new_reviews$listing_id)), ncol = length(unique(reviews_positive$word))))


# So we know what each column means we name the column by the word it represents

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


# We have essentially created a "BAG" of words.
# Each line is the unique listing, and we have a count of the number
# of times a word is used, we have a negative list and a positive list


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


# Let's just pick 10 clusters and see what happens
# When we build a word cloud

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




# Let's look at the negative words!!


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


# Let's just pick 10 clusters and see what happens
# When we build a word cloud

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


# narrow it down to words that were uses 10 or more times
temp_negative <- negative[, idx > 300]
# temp_negative <- temp_negative[,c(1,4,9,13)]


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


temp <- words_and_clusters %>%
  filter(group == 5) %>%
  count(word, sort = TRUE)


# finding the optimal number of clusters to cut
complete_Clust <- function(x,k){return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))}
average_Clust <- function(x,k){return(hcut(x,k, hc_method ="average" , hc_metric="euclidian"))}



# using mixed model clustering algo's -------------------------------------


fviz_nbclust(as.matrix(B), complete_Clust, method = "silhouette")
fviz_nbclust(as.matrix(B), average_Clust, method = "silhouette")




