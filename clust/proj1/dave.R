library(tidyverse)
library(tidytext)
library(SnowballC)
library(text2vec)

reviews <- read_csv("clust/data/reviews.csv")
str(reviews)

# how many unique listings are there
(n_ids <- n_distinct(reviews$listing_id)) # 2829


# building stratified training and testing sets based on the listing id column
train <- splitstackshape::stratified(reviews,
  size = .8,
  group = "listing_id"
)
set.seed(34)
train <- reviews %>%
  group_by(listing_id) %>%
  sample_frac(0.8) %>%
  ungroup()
test <- anti_join(reviews, train)

# checking we  maintained the same listing ids
sort(unique(train$listing_id)) %>% head()
sort(unique(test$listing_id)) %>% head()

train_token <- train %>%
  # using listing id as the document
  group_by(listing_id) %>%
  # getting the individual words from the comments
  unnest_tokens(word, comments) %>%
  # removing stop words from the comments
  anti_join(stop_words) %>%
  # regex to get only words (ie not numbers or characters)
  mutate(word = str_extract(word, "[a-z']+")) %>%
  # getting the stem of each word using only words in the english language
  mutate(word = wordStem(word, language = "english")) %>%
  # removing missing values
  filter(!is.na(word)) %>%
  # filter any hard coded na's
  filter(word != "NA") %>%
  # creating varible for count - n
  count(word, sort = TRUE) %>%
  # finding the proportion of each word
  mutate(p = n / sum(n)) %>%
  # remove words with very little frequency
  filter(p > 0.001) %>%
  ungroup()

head(train_token)


# building a term document - inverse document frequency dataframe
tdidf_train <- train_token %>%
  bind_tf_idf(word_stem, listing_id, n)

head(tdidf_train)

# lookin the the terms with the highest tf_idf score
tdidf_train %>%
  arrange((desc(tf_idf))) %>%
  head(10)

# looking at subset of 10 listing ids top 15 words by tf-idf
(samp_ids <- sample(train$listing_id, 10))
tdidf_train %>%
  filter(listing_id %in% samp_ids) %>%
  arrange((desc(tf_idf))) %>%
  head(15)

tdidf_train %>%
  filter(listing_id %in% samp_ids) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word_stem, levels = rev(unique(word_stem)))) %>%
  group_by(listing_id) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = factor(listing_id))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~listing_id, ncol = 2, scales = "free") +
  coord_flip()


# sentiment analysis of listings
# checking the 3 different sentiment lexicons
# afinn
train_token %>%
  filter(listing_id %in% samp_ids) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, sort = TRUE)

afinn <- train_token %>%
  filter(listing_id %in% samp_ids) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(listing_id) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

# bing and nrc
bing_and_nrc <- bind_rows(
  train_token %>%
    filter(listing_id %in% samp_ids) %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  train_token %>%
    filter(listing_id %in% samp_ids) %>%
    inner_join(get_sentiments("nrc") %>%
      filter(sentiment %in%
        c("positive", "negative"))) %>%
    mutate(method = "NRC")
) %>%
  count(method, listing_id, sentiment) %>%
  spread(sentiment, nn, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(
  afinn,
  bing_and_nrc
) %>%
  ggplot(aes(factor(listing_id), sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))



