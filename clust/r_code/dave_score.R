#######################################################
library(tidytext)
library(tidyverse)
library(magrittr)

##############################################
# Diferent Way to look at it
###############################################


# Load the dataset and group the
# reviews by the listing id
# keep all listings having more than 4 reviews
reviews <- read_csv("clust/data/reviews.csv")
listings <- read_csv("clust/data/listings.csv")
load("clust/data/MapListing.RData")


# break up the dataset's listing into individual words
# join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
# join to afinn lexicon library
nrc_total <- get_sentiments("afinn")


rev_4_plus <- reviews %>%
  group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>%
  filter(n >= 4) %>%
  select(-"n") %>% 
  pull

new_reviews <- reviews %>% 
  filter(listing_id %in% rev_4_plus) %>% 
  unnest_tokens(word, comments) %>% 
  filter(!is.na(word)) %>%
  left_join(nrc_total, by = "word") %>%
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

complete$avg <- scale(complete$avg) # standardize the score

listing_k %<>% 
  arrange(listing_id)

combined <- complete %>% 
  left_join(listing_k)

any(complete$listing_id %in% listing_k$listing_id)

combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

toC <- cbind(combined$avg, combined$std.lat, combined$std.lon)

clusters.c <- hclust(dist(toC), method = "complete")
clusters.s <- hclust(dist(toC), method = "single")
clusters.a <- hclust(dist(toC), method = "average")

plot(clusters.c, labels = FALSE)
plot(clusters.s, labels = FALSE)
plot(clusters.a, labels = FALSE)
combined$clus <- cutree(clusters.c, 6) # it looks like 4 clusters is reasonable

clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)

################################
# plot two interesting clusters
# 
# download a map of Boston
map <- get_map(location = "Boston", color = 'bw', zoom = 13)

ggmap(map) +
  geom_point(data = combined, aes(x = lon, y = lat, color = factor(clus), shape = factor(clus)), size = 3) +
  viridis::scale_color_viridis(discrete = TRUE)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu5, aes(x = lon, y = lat), color = "red", size = 2)

###############################
# describe them
###############################

# did as.data.frame because new_reviews
# was made to be a tibble with listing_id being a 'group'
# as.data.frame removes this distinction

words4 <- as.data.frame(new_reviews) %>%
  right_join(clu4, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)


words5 <- as.data.frame(new_reviews) %>%
  right_join(clu5, "listing_id") %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  filter(n < 150)

# random is not so random
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
