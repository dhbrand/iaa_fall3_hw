#######################################################3
#Analyze Seattle AirBNB dataset
#
#######################################################
library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)

#Load the dataset and group the 
#reviews by the listing id 
#keep all listings having more than 4 reviews 
reviews <- read_csv("reviews.csv")

rv <- reviews %>% group_by(listing_id) %>%
                  count(listing_id, sort = TRUE) %>% filter( n >= 4) %>% select(-"n") 

#break up the dataset's listing into individual words
#join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
new_reviews <- reviews %>%
               group_by(listing_id) %>%
               unnest_tokens(word, comments)  %>%
               right_join(rv,by="listing_id") %>% filter(!is.na(word))
                
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
                  
########################################################################
#look at all the unique positive and negative words that are in a review
pos_words <- sort(unique(reviews_positive$word))
neg_words <- sort(unique(reviews_negative$word))
#########################################################################
negative <- data.frame(matrix(0,nrow=length(unique(new_reviews$listing_id)),ncol = length(unique(reviews_negative$word))))
positive <- data.frame(matrix(0,nrow=length(unique(new_reviews$listing_id)),ncol = length(unique(reviews_positive$word))))

########################################################
# So we know what each column means we name the column by the word it represents
#########################################################
names(negative) <- neg_words
names(positive) <- pos_words

#create my bag of words matrices
lsid <- unique(new_reviews$listing_id)
# asign the number of positive words found
for (ii in 1:nrow(reviews_positive)){
    x <- which(pos_words == reviews_positive$word[ii])
    y <- which(lsid      == reviews_positive$listing_id[ii])
    positive[y,x] = reviews_positive$n[ii]
}

# asign the number of negative words found
for (ii in 1:nrow(reviews_negative)){
   x <- which(neg_words == reviews_negative$word[ii])
   y <- which(lsid      == reviews_negative$listing_id[ii])
   negative[y,x] = reviews_negative$n[ii]
}

#######################################################
# We have essentially created a "BAG" of words. 
# Each line is the unique listing, and we have a count of the number
# of times a word is used, we have a negative list and a positive list
#######################################################

#COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
library(Matrix)
SP.COS <- 1-sim2(Matrix(as.matrix(positive)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(positive)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
                                          #it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations


POSITIVE_CLUSTERS_COS  <- hclust(as.dist(B))  # CLUSTER as.dist(B) convert it to a 'distance'
POSITIVE_CLUSTERS_JAC  <- hclust(as.dist(C))  # because again hclust is stupid

#####################################################
#Let's just pick 10 clusters and see what happens
#When we build a word cloud
#####################################################
POSITIVE_GROUPS <- cutree(POSITIVE_CLUSTERS_COS,10)
listing_group <- as.data.frame(cbind(POSITIVE_GROUPS,lsid))
names(listing_group) <- c("group","listing_id")

words_and_clusters <- reviews_positive %>% right_join(listing_group,'listing_id')

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1:10){
  temp <- words_and_clusters %>% group_by('word') %>% 
                                 filter(group == ii) %>%
                                 count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(words = temp$word, freq = temp$nn, min.freq = 500,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}



#######################################################
# Let's look at the negative words!!
#
#
#######################################################

#COMPUTE THE TWO POPULAR DISTANCE MATRICES FOR TEXT BASED ANALYSIS
library(Matrix)
SP.COS <- 1-sim2(Matrix(as.matrix(negative)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(negative)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
#it needs to know the number of observations


NEGATIVE_CLUSTERS_COS  <- hclust(as.dist(B),method="single")  # CLUSTER as.dist(B) convert it to a 'distance'
NEGATIVE_CLUSTERS_JAC  <- hclust(as.dist(C),method="single")#ecause again hclust is stupid

#####################################################
#Let's just pick 10 clusters and see what happens
#When we build a word cloud
#####################################################
NEGATIVE_GROUPS <- cutree(NEGATIVE_CLUSTERS_COS,20)
listing_group <- as.data.frame(cbind(NEGATIVE_GROUPS,lsid))
names(listing_group) <- c("group","listing_id")

words_and_clusters <- reviews_negative %>% right_join(listing_group,'listing_id')

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1){
  temp <- words_and_clusters %>% group_by('word') %>% 
    filter(group == ii) %>%
    count(word, sort = TRUE)
  set.seed(8675309)
  wordcloud(words = temp$word, freq = temp$nn, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}

##############################
# 
##############################
#narrow it down to words that were uses 10 or more times
temp_negative <- negative[,idx > 300] 
#temp_negative <- temp_negative[,c(1,4,9,13)]
library(tidyverse)
library(knitr)
library(factoextra)
temp_negative <- scale(temp_negative)
SP.COS <- 1-sim2(Matrix(as.matrix(temp_negative)),method="cosine", norm = "l2")
SP.JAC <- 1-sim2(Matrix(as.matrix(temp_negative)),method="jaccard")

B <- SP.COS
attr(B,"Size") <- as.integer(nrow(B))    ##Need to do this because R hclust is stupid
                                         ##it needs to know the number of observations

C <- SP.JAC
attr(C,"Size") <- as.integer(nrow(C))    ##Need to do this because R hclust is stupid
                                         ##it needs to know the number of observations


NEGATIVE_CLUSTERS_COS  <- hclust(as.dist(B),method="average")  # CLUSTER as.dist(B) convert it to a 'distance'
#NEGATIVE_CLUSTERS_JAC  <- hclust(as.dist(C),method="average")#ecause again hclust is stupid


NEGATIVE_GROUPS <- cutree(NEGATIVE_CLUSTERS_COS,15)
listing_group <- as.data.frame(cbind(NEGATIVE_GROUPS,lsid))
names(listing_group) <- c("group","listing_id")

words_and_clusters <- reviews_negative %>% right_join(listing_group,'listing_id')

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

for (ii in 1:15){
  temp <- words_and_clusters %>% group_by('word') %>% 
                                 filter(group == ii) %>%
                                 count(word, sort = TRUE)
  set.seed(8675309)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0,4))
  plot.new()
  text(x=0.5, y=0.5, sprintf("Group #%d",ii))
  wordcloud(words = temp$word, freq = temp$nn, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}

################################################
## CLUSTER 5 IS INTERESTING "DIE" IS THE MOST USED TERM ...
############################################

temp <- words_and_clusters %>% filter(group == 5) %>%
        count(word, sort = TRUE)



