###################################
## Basic Text Processing with R
##
##################################

library(gutenbergr)
###########################################
#We are going to use tidy text mining
#exclusively in this class
library(tidytext)
library(dplyr)
library(stringr)
library(SnowballC)
###############################
#intro to dplyr & tidy

washington <- gutenberg_works(author == "Washington, George") 
jefferson  <- gutenberg_works(author == "Jefferson, Thomas")
lincoln    <- gutenberg_works(author == "Lincoln, Abraham")

washington_sou <- gutenberg_download(5010)
jefferson_sou  <- gutenberg_download(5012)
lincoln_sou    <- gutenberg_download(5024)


#break up the documents remove 'stop words' 
wash_token    <-  washington_sou %>% unnest_tokens(word, text) %>%
                  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
                  mutate(word_stem = wordStem(word, language="english")) %>%
                  filter(word_stem != "NA") %>%
                  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
jeff_token    <- jefferson_sou %>%  unnest_tokens(word, text) %>%
                  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
                  mutate(word_stem = wordStem(word, language="english")) %>%
                  filter(word_stem != "NA") %>%
                  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
linc_token    <- lincoln_sou %>%  unnest_tokens(word, text) %>%
                  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
                  mutate(word_stem = wordStem(word, language="english")) %>%
                  filter(word_stem != "NA") %>%
                  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
##################################################
#we have created three 'bags of words' for each state of the 
#union adress. 

u_words <- sort(unique(c(wash_token$word_stem,jeff_token$word_stem,linc_token$word_stem)))

bag_ow <- as.data.frame(matrix(0,nrow=3,ncol=length(u_words))) # make a matrix whose colums are the words
                                                # and each row is a state of the union address
names(bag_ow) <- u_words

for (ii in 1:nrow(wash_token)) {
    idx <- which(wash_token$word_stem[ii] == u_words)
    bag_ow[1,idx] = wash_token$p[ii]
}


for (ii in 1:nrow(linc_token)) {
     idx <- which(linc_token$word_stem[ii] == u_words)
     bag_ow[2,idx] = linc_token$p[ii]
}


for (ii in 1:nrow(jeff_token)) {
     idx <- which(jeff_token$word_stem[ii] == u_words)
     bag_ow[3,idx] = jeff_token$p[ii]
}

 
dist(bag_ow,diag = TRUE) #euclidean
library(text2vec)
X <- dist2(as.matrix(bag_ow)) #cosine
attr(X,"Size") <- as.integer(nrow(X))   
clusters <- hclust(as.dist(X))
plot(clusters)
###########################################################
# Build a word cloud looking at common words between Jefferson and Washington as they are 
# "Closest"
comb_token <- rbind(wash_token,jeff_token)

#
#
###########################################################

#reweight completely ignoring there were other words
rw_bag_ow = bag_ow
qr_bag_ow = bag_ow
for (ii in 1:nrow(bag_ow)){
  rw_bag_ow[ii,] = bag_ow[ii,]/sum(bag_ow[ii,])
  
}


dist(rw_bag_ow,diag=TRUE)   #this is different Why?
dist2(as.matrix(rw_bag_ow)) # notice this is the same as above Why?

###############
# What happens when we remove words that only are spoke by one author ?
# Let's code this...
##############

#####################################
#NOW YOU DO the analysis and cluster the following works by Shakespeare
#if you are bold, you can add more books without too much trouble
#COMPARE the cosine distance of  
#Shakespeare's tragedies       : Romeo and Julliette, Hamlet, Macbeth
#  to   Shakespeare's comedies : A Midsummer Night's Dream, Much Ado About Nothing, 
#                              : The Taming of the Shrew 
library(stringr)
c("Romeo and Julliette", "Hamlet")
shakespeare <- gutenberg_works(author == "Shakespeare, William") #Works of shakespeare
shakespeare_books <- gutenberg_download(c(1513,1514,1519,1524,1533,1508))


gberg_token    <- shakespeare_books %>%  group_by(gutenberg_id) %>% unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
                mutate(word_stem = wordStem(word, language="english")) %>%
                filter(word_stem != "NA") %>%
                count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)


u_words <- sort(unique(gberg_token$word_stem))

bag_ow <- as.data.frame(matrix(0,nrow=6,ncol=length(u_words))) # make a matrix whose colums are the words
# and each row is a state of the union address
names(bag_ow) <- u_words

unique_id <- sort(unique(gberg_token$gutenberg_id))
k<-1

for (jj in unique_id){
  temp <- gberg_token %>% filter(gutenberg_id == jj)
  for (ii in 1:nrow(temp)) {
    idx <- which(temp$word_stem[ii] == u_words)
    bag_ow[k,idx] = temp$p[ii]
  }
  k <- k+1
}
dists <- dist(bag_ow)
dists <- dist2(as.matrix(bag_ow))
attr(dists,"Size") <- as.integer(nrow(dists))   
clusters <- hclust(as.dist(dists))
plot(clusters)
  ##


##################################################



