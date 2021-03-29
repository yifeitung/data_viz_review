library(tidyverse)
library(tidytext)
library(ggthemes)

# Text as data
review_data <- read_csv("dataset/Roomba Reviews.csv")
review_data

review_data %>%
  filter(Product == "iRobot Roomba 650 for Pets") %>%
  summarise(stars_mean = mean(Stars))

review_data %>%
  group_by(Product) %>%
  summarise(stars_mean = mean(Stars))

# Airline tweets data
# The twitter data frame has over 7000 tweets about airlines. The tweets have alreadly been classified as either complaints or non-complaints.
twitter_data <- readRDS("dataset/ch_1_twitter_data.rds")
str(twitter_data)

# Print just the complaints in twitter data
twitter_data %>%
  filter(complaint_label == "Complaint") %>%
  nrow()
# [1] 1676

# Compute the average, minimum, and maximum number of user followers count
twitter_data %>%
  group_by(complaint_label) %>%
  summarise(
    avg_followers = mean(usr_followers_count), 
    min_followers = min(usr_followers_count),
    max_followers = max(usr_followers_count)
  )


review_data %>%
  count(Product)

# Counting user types
twitter_data %>%
  # Filter for just the complaints
  filter(complaint_label == "Complaint") %>%
  # Count the number of verified and non-verified users
  count(usr_verified)

# Verified Twitter users complain less often than non-verified Twitter users

twitter_data %>%
  # Group by whether or not a user is verified
  group_by(usr_verified) %>%
  summarise(
    # Compute the average number of followers
    avg_followers = mean(usr_followers_count), 
    # Count the number of users in each category
    n = n())

# There are fewer verified users. 
# We can also see that they, on average, have far more followers than non-verified users. 

# Tokenizing and cleaning
# Tokenizing text
# Some natural language processing (NLP) vocabulary
# Bag of words: Words in a document are independent
# Every separate body of text is a document
# Every unique word is a term
# Every occurrence of a term is a token

tidy_review <- review_data %>%
  unnest_tokens(word, Review)

tidy_review
# punctuation is gone, each word is lowercase, and white space has been removed.

# Counting words
tidy_review %>%
  count(word) %>%
  arrange(desc(n))

# anti_join()
# These common and uninformative words are known as stop words and we'd like to remove them from our tidied data frame
tidy_review2 <- review_data %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words, by = "word")

tidy_review2

tidy_review2 %>%
  count(word) %>%
  arrange(desc(n))

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) 

tidy_twitter %>% 
  # Compute word counts
  count(word) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))

tidy_twitter <- twitter_data %>%
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) %>%
  # Remove stop words
  anti_join(stop_words, by = "word")

tidy_twitter %>%
  # Filter to keep complaints only
  filter(complaint_label == "Complaint") %>%
  # Compute word counts and arrange in descending order
  count(word) %>%
  arrange(desc(n))

# It looks like complaints include frequent references to time, delays, and service. 
# However, there are simply a lot of specific airlines referenced. These could be considered as stop words specific to this data.

# Plotting word counts
tidy_review <- review_data %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words, by = "word")

tidy_review

word_counts <- tidy_review %>%
  count(word) %>%
  arrange(desc(n))

# ggplot(word_counts, aes(x = word, y = n))+
#   geom_bar(stat = "identity")
# The above will take long time and plot all words

word_counts2 <- tidy_review %>%
  count(word) %>%
  filter(n > 300) %>%
  arrange(desc(n))

word_counts2 %>%
  ggplot(aes(x = fct_reorder(word, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_bw()

tidy_twitter <- twitter_data %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(stop_words, by = "word")

word_counts <- tidy_twitter %>%
  filter(complaint_label == "Complaint") %>%
  count(word) %>%
  # Keep words with count greater than 100
  filter(n > 100)

# Create a bar plot using word_counts with x = word
word_counts %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_bw()

# Visualizing non-complaints
word_counts <- tidy_twitter %>%
  filter(complaint_label == "Non-Complaint") %>%
  count(word) %>%
  filter(n > 150)

word_counts %>%
  ggplot(aes(x = fct_reorder(word, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_bw()+
  ggtitle("Non-Complaint Word Counts")

# Improving word count plots
# Custom stop words
# The easiest way to do this is to first create our own data frame, or tibble. 
# To do this, we use the tribble() function. If you get the Star Trek reference, congratulations. The arguments in tribble() are simple: the column names, with the tilde in front of them, followed by the values on each row. 

tribble(
  ~word, ~lexicon,
  "roomba", "CUSTOM",
  "2", "CUSTOM"
)

custom_stop_words <- tribble(
  ~word, ~lexicon,
  "roomba", "CUSTOM",
  "2", "CUSTOM"
)

stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

# Removing stop words again
tidy_review <- review_data %>%
  mutate(id = row_number()) %>%
  dplyr::select(id, "date" = Date, "product" = Product, "stars" = Stars, "review" = Review) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words2, by = "word")

tidy_review %>%
  filter(word == "roomba")

# Adding custom stop words
custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "win", "CUSTOM",
  "t.co", "CUSTOM"
)

# Bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

tidy_twitter <- twitter_data %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(stop_words2, by = "word")

tidy_twitter %>%
  filter(complaint_label == "Non-Complaint") %>%
  count(word) %>%
  filter(n > 100) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Non-Compaint Word Counts")+
  theme_bw()

# Counting by product
tidy_review %>%
  count(word, product) %>%
  arrange(desc(n)) %>%
  group_by(product) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(word, n), y = n, fill = product))+
  geom_bar(stat = "identity")+
  facet_wrap(~product, scales = "free_y")+
  coord_flip()+
  theme_bw()+
  guides(fill = FALSE)

# Plotting Word Clouds
library(wordcloud)

word_counts <- tidy_review %>%
  count(word)

wordcloud(
  words = word_counts$word, 
  freq = word_counts$n,
  max.words = 30
)

# If we call that same code again we can see that the size of each of the words stays the same while the location of words in the cloud changes. This is because the size of each the words in the cloud is based on the relative word count, which is fixed. Meanwhile, where each word is located in the cloud is randomized each time the function is called. 


wordcloud(
  words = word_counts$word, 
  freq = word_counts$n,
  max.words = 30, 
  colors = "blue"
)

# Sentiment Dictionaries
# Bing dictionary
get_sentiments("bing")

get_sentiments("bing") %>%
  count(sentiment)

get_sentiments("afinn") %>%
  summarise(min = min(value), 
            max = max(value))

get_sentiments("loughran") %>%
  count(sentiment) %>%
  ggplot(aes(x = fct_reorder(sentiment, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_bw()+
  labs(x = "Sentiment", 
       y = "Count", 
       title = "Sentiment Counts in Loughran")

# Counting the NRC sentiments
get_sentiments("nrc") %>%
  count(sentiment) %>%
  ggplot(aes(x = fct_reorder(sentiment, n), y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Sentiment Counts in NRC", 
       x = "Sentiment", 
       y = "Count")+
  theme_bw()

# Appending dictionaries
# Using inner_join()

tidy_review %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(word, sentiment) %>%
  arrange(desc(n))

# Visualizing sentiment
# Only Positive and Negative
tidy_review %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(word, n), y = n, fill = sentiment))+
  geom_bar(stat = "identity")+
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()+
  guides(fill = FALSE)+
  labs(title = "Sentiment Word Counts", 
       x = "Sentiment", 
       y = "Count")

# Here we can see that the negative words are used to describe difficulties cleaning while the positive words describe reactions to the performance. 

custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "win", "CUSTOM",
  "t.co", "CUSTOM",
  "united", "CUSTOM",
  "flying", "CUSTOM",
)

stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

tidy_twitter <- twitter_data %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(stop_words2, by = "word")

tidy_twitter %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(sentiment) %>%
  arrange(desc(n))

tidy_twitter %>% 
  # Append the NRC dictionary and filter for positive, fear, and trust
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(sentiment %in% c("positive", "fear", "trust")) %>%
  # Count by word and sentiment and keep the top 10 of each
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, wt = n) %>% 
  ungroup() %>% 
  # Create a factor called word2 that has each word ordered by the count
  # mutate(word2 = fct_reorder(word, n)) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n, fill = sentiment))+
  geom_bar(stat = "identity")+
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()+
  guides(fill = FALSE)+
  labs(
    title = "Sentiment Word Counts",
    x = "Words",
    y = "Count"
  )

# Remember, our sentiment analysis is conditioned on the dictionary we use. 
# It's a tall order, but finding or building a sentiment dictionary that is context-specific would be ideal. 

# Improving Sentiment Analysis
# Count sentiment by rating
tidy_review %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(stars, sentiment)

# Use spread()
tidy_review %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(stars, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(overall_sentiment = positive - negative)

tidy_review %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(stars, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(overall_sentiment = positive - negative) %>%
  ggplot(aes(x = fct_reorder(as.factor(stars), overall_sentiment), y = overall_sentiment, fill = as.factor(stars)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  guides(fill = guide_legend(title = "Stars"))+
  labs(title = "Overall Sentiment by Stars", 
       subtitle = "Reviews for Robotic Vaccums",
       x = "Stars", 
       y = "Overall Sentiment")

# 5-star rating overwhelmingly positive, but a rating of 1 to 4 isn’t really positive at all, with a 4-star rating providing neutral overall sentiment at best.


tidy_twitter %>% 
  # Append the NRC sentiment dictionary
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  # Count by complaint label and sentiment
  count(complaint_label, sentiment) %>% 
  # Spread the sentiment and count columns
  spread(sentiment, n)

tidy_twitter %>% 
  # Append the afinn sentiment dictionary
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  # Group by both complaint label and whether or not the user is verified
  group_by(complaint_label, usr_verified) %>% 
  # Summarize the data with an aggregate_value = sum(value)
  summarise(aggregate_value = sum(value)) %>% 
  # Spread the complaint_label and aggregate_value columns
  spread(complaint_label, aggregate_value) %>% 
  mutate(overall_sentiment = Complaint + `Non-Complaint`)

tidy_twitter %>% 
  # Append the bing sentiment dictionary
  inner_join(get_sentiments("bing"), by = "word") %>% 
  # Count by complaint label and sentiment
  count(complaint_label, sentiment) %>% 
  # Spread the sentiment and count columns
  spread(sentiment, n) %>% 
  # Compute overall_sentiment = positive - negative
  mutate(overall_sentiment = positive-negative) %>% 
  # Create a bar plot out of overall sentiment by complaint label, colored by complaint label as a factor
  ggplot(aes(x = fct_reorder(complaint_label, overall_sentiment), 
             y = overall_sentiment, 
             fill = factor(complaint_label)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  guides(fill = FALSE)+
  labs(title = "Overall Sentiment by Complaint Label", 
       subtitle = "Airline Twitter Data",
       x = "",
       y = "Overall Sentiment")
  
# Topic Modeling: Latent Dirichlet Allocation
# Unsupervised learning
# Latent Dirichlet Allocation Topic Model searches for patterns of words occuring together within and across a collection documents, also known as corpus. LDA finds topics in a corpus by creating a separate bag for each document and dumping the words out to look for patterns in which words appear together -- not just in one bag, but consistently across all the document-specific bags in our corpus. Note that LDA isn’t trying to explain or predict any dependent variable, like in a regression. It's simply looking for patterns within a group of explanatory variables. This is known as unsupervised learning.

# Clustering vs. Topic Modeling
# Clustering
# Clusters are uncovered based on distance, which is continuous
# Every object is assigned to a single cluster

# Topic Modeling
# Topics are uncovered based on word frequency, which is discrete
# Every document is a mixture (partial member) of every topic.

# Document Term matrices
# To run a topic model, we first need to create a document term matrix or DTM. 
# Matrices and sparsity
# We can see that a matrix is like a data frame, except every column has to be of the same type. We index a matrix by referring to its rows and then its columns. A document term matrix has a single row for each document and a column for every unique word or term used across all documents in the corpus. The values in the DTM are the count of tokens or uses of each term for the given document. 

# When you have a matrix that is composed mostly of zeros, this is referred to as sparsity or a sparse matrix. Our DTM will likely be sparse, since most documents don’t use most of the terms that are present across the corpus.

# Create a DTM
tidy_review %>%
  count(word, id) %>%
  cast_dtm(id, word, n) # Document column, the term column, and word count

# There is also a reference to sparsity, or how many of the entries in this matrix are non-zero. As we might expect, the DTM is very sparse. 

# Explore DTM
dtm_review <- tidy_review %>%
  count(word, id) %>%
  cast_dtm(id, word, n) %>%
  as.matrix()

# Look part of matrix
dtm_review[1:4, 2000:2004]

# Start with the tidied Twitter data
tidy_twitter %>% 
  # Count each word used in each tweet
  count(word, tweet_id) %>% 
  # Use the word counts by tweet to create a DTM
  cast_dtm(tweet_id, word, n)

# Assign the DTM to dtm_twitter
dtm_twitter <- tidy_twitter%>% 
  count(word, tweet_id) %>% 
  # Cast the word counts by tweet into a DTM
  cast_dtm(tweet_id, word, n)

# Coerce dtm_twitter into a matrix called matrix_twitter
matrix_twitter <- as.matrix(dtm_twitter)

# Print rows 1 through 5 and columns 90 through 95
matrix_twitter[1:5, 90:95]

# Running topic models
library(topicmodels)

lda_out <- LDA(
  dtm_review,
  k = 2, # Number of topics
  method = "Gibbs", # Estimate Method
  control = list(seed = 42) # Specify simulating seed
)

glimpse(lda_out)

lda_topics <- lda_out %>%
  tidy(matrix = "beta")

lda_topics %>%
  arrange(desc(beta))

# Run an LDA with 2 topics and a Gibbs sampler
lda_out2 <- LDA(
  dtm_twitter,
  k = 2,
  method = "Gibbs",
  control = list(seed = 42)
)

lda_topics2 <- lda_out2 %>%
  tidy(matrix = "beta")

lda_topics2 %>%
  arrange(desc(beta))

# Interpreting topics
# interpreting a topic model is something of an art form. Much like clustering and other unsupervised learning techniques, we get a description of what each topic is composed of but no other direction as to what the topics mean. The key is to find topics that are each different where the topics don’t repeat. 

# The art of model selection
# Adding topics that are different is good
# If we start repeating topics, we've gone too far
# Name the topics based on the combination of high-probability words

# Summary
# Tokenizing text and removing stop words
# Visualizing word counts
# Conducting sentiment analysis
# Running and interpreting topic models

# Sentiment Analysis in R: The Tidy Way
# Topic Modeling in R




