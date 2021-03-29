# Text Mining with Bag-of-Words in R

# What is text mining?
# The process of distilling actionable insights from text

# Text Mining workflow
# 1. Problem definition & specific goals
# 2. Identify text to be collected
# 3. Text organization
# 4. Feature extraction
# 5. Analysis
# 6. Reach an insight, recommendation, or output

# Two approaches
# Semantic parsing vs. bag of words
# Semantic parsing: tree structure
# Bag of words: doesn't care about word type or order. Words are just attributes of the document. Treats each term as just a single token in the sentence no matter the type or order.

# Load packages
library(qdap)
library(tidytext)
library(tidyverse)
library(ggthemes)

new_text <- "DataCamp is the first online learning platform that focuses on building the best learning experience specifically for Data Science. We have offices in New York, London, and Belgium, and to date, we trained over 3.8 million (aspiring) data scientists in over 150 countries. These data science enthusiasts completed more than 185 million exercises. You can take free beginner courses, or subscribe for $29/month to get access to all premium courses."

# Find the 10 most frequent terms
term_count <- freq_terms(new_text, top = 10)

# Plot term_count
plot(term_count)

# Building our first corpus
# Load corpus
coffee_tweets <- read.csv("dataset/coffee.csv", stringsAsFactors = FALSE)

# Vector of tweets
coffee_tweets <- coffee_tweets$text

# View first 5 tweets
head(coffee_tweets, 5)

# Convert this vector containing the text data to a corpus. A corpus is a collection of documents, but it's also important to know that in the tm domain, R recognizes it as a data type.
library(tm)

# There are two kinds of the corpus data type, the permanent corpus, PCorpus, and the volatile corpus, VCorpus. In essence, the difference between the two has to do with how the collection of documents is stored on your computer. In this course, we will use the volatile corpus, which is held in your computer's RAM rather than saved to disk, just to be more memory efficient.

# To make a volatile corpus, R needs to interpret each element in our vector of text as document. And use VectorSource() in tm package.

# Make a vector source 
coffee_source <- tm::VectorSource(coffee_tweets)
class(coffee_source)
# [1] "VectorSource" "SimpleSource" "Source"

# Make a volatile corpus from coffee_source
coffee_corpus <- tm::VCorpus(coffee_source)
# The VCorpus object is a nested list or list of lists. At each index of the VCorpus object, there is a PlainTextDocument object, which is a list containing actual text data (content), and some corresponding metadata (meta).

# To review a single document object (the 10th), you subset with double square brackets
coffee_corpus[[10]]

# To review the actual text, you index the list twice. To access the document's metadata, you neet another square bracket.
# The content
coffee_corpus[[10]][1]

# The metadata
coffee_corpus[[10]][2]

# To view plain text use context()
content(coffee_corpus[[10]])

# Make a VCorpus from a data frame
# If your text data is in a data frame, you can use DataframeSource() for your analysis. The data frame passed to DataframeSource() must have a specific structure:

# Column one must be called doc_id and contain a unique string for each row.
# Column two must be called text with "UTF-8" encoding (pretty standard).
# Any other columns, 3+, are considered metadata and will be retained as such.

# use meta() to extract the metadata associated with each document
# Often your data will have metadata such as authors, dates, topic tags, or places that can inform your analysis. Once your text is a corpus, you can apply meta() to examine the additional document level information.

example_text <- data.frame(
  doc_id = c(1, 2, 3),
  text = c("Text mining is a great time.", 
           "Text analysis provides insights", 
           "qdap and tm are used in text mining"),
  author = c("Author1", 
             "Author2",
             "Author3"),
  date = c("1514953399", 
           "1514866998",
           "1514780598")
)

# Create a DataframeSource from the example text
df_source <- DataframeSource(example_text)

# Convert df_source to a volatile corpus
df_corpus <- tm::VCorpus(df_source)

# Examine df_corpus
df_corpus

# Examine df_corpus metadata
meta(df_corpus)

# Compare metadata in the vector corpus
meta(coffee_corpus)

# Cleaning and preprocessing text
# Applying various preprocessing functions use tm_map()
tm::tm_map(coffee_corpus, removeNumbers)
tm::tm_map(coffee_corpus, removePunctuation)
tm::tm_map(coffee_corpus, content_transformer(replace_abbreviation))

# Another preprocessing setp: word stemming
# Stem words
stem_words <- stemDocument(c("complicatedly", "complicated", "complication"))
stem_words
# [1] "complic" "complic" "complic"

# The problem is that you are often left with tokens that are not words! So you have to take an additional step to complete the base tokens. 

# Complete words using single word directory
stemCompletion(stem_words, c("complicate")) # the second argument is a directory of complete words
#     complic      complic      complic 
# "complicate" "complicate" "complicate" 

text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer."

# Make lowercase
tolower(text)

# Remove punctuation
removePunctuation(text)

# Remove numbers
removeNumbers(text)

# Remove whitespace
stripWhitespace(text)

# Clean with qdap package
# bracketX(): Remove all text within brackets (e.g. "It's (so) cool" becomes "It's cool")
# replace_number(): Replace numbers with their word equivalents (e.g. "2" becomes "two")
# replace_abbreviation(): Replace abbreviations with their full text equivalents (e.g. "Sr" becomes "Senior")
# replace_contraction(): Convert contractions back to their base words (e.g. "shouldn't" becomes "should not")
# replace_symbol() Replace common symbols with their word equivalents (e.g. "$" becomes "dollar")

# Remove text within brackets
bracketX(text)

# Replace numbers with words
replace_number(text)

# Replace abbreviations
replace_abbreviation(text)

# Replace contractions
replace_contraction(text)

# Replace symbols with words
replace_symbol(text)

# Stop words
# stop words in tm package
# In the tm package, there are 174 common English stop words 
# When you are doing an analysis, you will likely need to add to this list.

# Once you have a list of stop words that makes sense, you will use the removeWords() function on your text. removeWords() takes two arguments: the text object to which it's being applied and the list of words to remove.

# List standard English stop words
stopwords("en")

# Print text without standard stop words
removeWords(text, stopwords("en"))

# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))

# Remove stop words from text
removeWords(text, new_stops)

# Intro to word stemming and stem completion

# The tm package provides the stemDocument() function to get to a word's root. This function either takes in a character vector and returns a character vector, or takes in a PlainTextDocument and returns a PlainTextDocument.

# Create complicate
complicate <- c("complicated", "complication", "complicatedly")

# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)

# Create the completion dictionary: comp_dict
comp_dict <- c("complicate")

# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)

# Print complete_text
complete_text

text_data <- "In a complicated haste, Tom rushed to fix a new complication, too complicatedly."

# This sentence contains the same three forms of the word "complicate" that we saw in the previous exercise. The difference here is that even if you called stemDocument() on this sentence, it would return the sentence without stemming any words. Take a moment and try it out in the console. Be sure to include the punctuation marks.

stemDocument(text_data) # This does not work

# This happens because stemDocument() treats the whole sentence as one word. In other words, our document is a character vector of length 1, instead of length n, where n is the number of words in the document. To solve this problem, we first remove the punctuation marks with the removePunctuation() function.

# We then strsplit() this character vector of length 1 to length n, unlist(), then proceed to stem and recomplete

# Remove Punctuation
rm_punc <- removePunctuation(text_data)
# [1] "In a complicated haste Tom rushed to fix a new complication too complicatedly"

# Create character vector
n_char_vec <- unlist(strsplit(rm_punc, split = " "))

# Perform word stemming
stem_doc <- stemDocument(n_char_vec)

stem_doc

comp_dict <- c("In", "a", "complicate", "haste", "Tom", "rush", 
               "to", "fix", "new", "too")

# Re-complete stemmed document
complete_doc <- stemCompletion(stem_doc, dictionary = comp_dict)

# Applying preprocessing steps to a corpus

# The tm package provides a function tm_map() to apply cleaning functions to an entire corpus, making the cleaning steps easier. 

# For compatibility, base R and qdap functions need to be wrapped in content_transformer()

clean_corpus <- function(corpus){
  # Remove Punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transfer to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, word = c(stopwords("en"), 
                                                 "coffee", 
                                                 "mug"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}


# Apply customized function to the tweet_corp
tweet_corp <- coffee_corpus

clean_corp <- clean_corpus(tweet_corp)

# Print the cleanned 227th tweet
content(clean_corp[[227]])
# [1] "also dogs arent smart enough dip donut eat part thats dipped ladyandthetramp"

# Print out the same tweet in the original form
tweets <- read.csv("dataset/coffee.csv", stringsAsFactors = FALSE)

tweets$text[227]
# [1] "Also, dogs aren't smart enough to dip the donut in the coffee and then eat the part that's been dipped. #ladyandthetramp"

# The TDM & DTM
# Term Document Matrix or Document Term Matrix
# Term Document Matrix has each corpus word represented as a row with documents as column

# Generate TDM
coffee_tdm <- tm::TermDocumentMatrix(clean_corp)

# Document Term Matrix
# The document term matrix is the transposition of the TDM so each document is a row and each word is a column. 

# Generate DTM
coffee_dtm <- tm::DocumentTermMatrix(clean_corp)
# In its simplest form, the matrices contain word frequencies. However, other frequency measures do exit

# qdap package can use to generate word frequency matrix
coffee_wfm <- qdap::wfm(tweets$text)
# It is less popular.

# Convert coffee_dtm to a matrix
coffee_m <- as.matrix(coffee_dtm)

# Review a portion of the matrix to get some Starbucks
coffee_m[25:35, c("star", "starbucks")]

# Convert coffee_tdm to a matrix
coffee_m2 <- as.matrix(coffee_tdm)

# Print the dimensions of the matrix
dim(coffee_m2)

# Review a portion of the Term Document Matrix
coffee_m2[c("star", "starbucks"), 25:35]

# Word clouds and more interesting visuals
# Why make visuals?
# Good visuals lead to quick conclusions
# The brain efficiently processes visual information

# Term frequency plots with tm package
# Use Term Document Matrix
library(Matrix)
term_frequency <- Matrix::rowSums(coffee_m2)
term_frequency <- sort(term_frequency, 
                       decreasing = TRUE)

# Create a barplot
barplot(term_frequency[1:10], 
        col = "tan", 
        las = 2)

# Term frequency plots with qdap package
frequency <- qdap::freq_terms(
  text.var = tweets$text, # Input a text vector 
  top = 10, # Specifies 10 words to draw
  at.least = 3, # A word should only appear if it occurs at least 3 character length
  stopwords = "Top200Words"
)

# Check Top 200 Words
data("Top200Words")

plot(frequency)

# Intro to word clouds
# A simple word cloud
# Have had too much coffee. Plus, seeing the top words such as "shop", "morning", and "drinking" among others just isn't all that insightful. So, we should remove those words

clean_corpus <- function(corpus){
  # Remove white spaces
  corpus <- tm::tm_map(corpus, stripWhitespace)
  # Remove numbers
  corpus <- tm::tm_map(corpus, removeNumbers)
  # Replace contractions
  corpus <- tm::tm_map(corpus, content_transformer(replace_contraction))
  # Transform to lower case
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  # Remove some words
  corpus <- tm::tm_map(corpus, removeWords, c(stopwords("en"), 
                                              "coffee", 
                                              "amp",
                                              "mug", 
                                              "morning", 
                                              "drinking",
                                              "wine", 
                                              "chardonnay",
                                              "glass"))
  # Remove Punctuation
  corpus <- tm::tm_map(corpus, removePunctuation)
  return(corpus)
}

# Apply clean corpus function to tweets
clean_corp <- clean_corpus(tweet_corp)

content(clean_corp[[1]])

coffee_tdm <- TermDocumentMatrix(clean_corp)
coffee_term_matrix <- as.matrix(coffee_tdm)

term_frequency <- Matrix::rowSums(coffee_term_matrix)
terms_vector <- names(term_frequency)
library(wordcloud)
wordcloud(
  words = terms_vector,
  freq = term_frequency, 
  max.words = 50,
  colors = "red"
)

# Improve word cloud colors
# Print the lost of colors
colors()

wordcloud(
  words = terms_vector, 
  freq = term_frequency, 
  max.words = 100,
  colors = c("#ece7f2", "#a6bddb", "#2b8cbe")
  # Doing so will naturally divide the term frequency into "low", "medium", "high" for easier viewing
)

# Use prebuilt color palettes
library(viridisLite)
# Select 5 colors
color_pal <- viridis(n = 5)

# Create a word cloud with the selected palette
wordcloud(
  words = terms_vector,
  freq = term_frequency,
  max.words = 100,
  colors = color_pal
)

# Other word clouds and word networks
# Comparison clouds
chardonnay_tweets <- read.csv("dataset/chardonnay.csv", 
                       stringsAsFactors = FALSE)

coffee_tweets <- read.csv("dataset/coffee.csv", 
                          stringsAsFactors = FALSE)

all_chardonnay <- paste(chardonnay_tweets$text,
                        collapse = "")

all_coffee <- paste(coffee_tweets$text,
                    collapse = "")

all_tweets <- c(all_coffee, all_chardonnay)

# Clean all tweets
all_tweets <- tm::VectorSource(all_tweets)
all_corpus <- tm::VCorpus(all_tweets)
all_clean <- clean_corpus(all_corpus)
all_tdm <- TermDocumentMatrix(all_clean)
colnames(all_tdm) <- c("coffee", "chardonnay")
all_m <- as.matrix(all_tdm)

# Make Comparison cloud
comparison.cloud(all_m, 
                 colors = c("orange", "blue"),
                 max.words = 50)
# The comparison cloud function will identify the words that are dissimilar

# Commodity Clouds
commonality.cloud(all_m,
                  colors = "steelblue1",
                  max.words = 100)

# Pyramid Plots
# Another way to visualize the conjunction of two corpora is with a polarized tag plot

# Identify terms shared by both documents
common_words <- subset(
  all_m,
  all_m[, 1] >0 & all_m[, 2] > 0 
)

# Find most commonly shared words
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3], 
                                   decreasing = FALSE), ]
top25_df <- data.frame(
  x = common_words[1:25, 1],
  y = common_words[1:25, 2],
  labels = rownames(common_words[1:25, ])
)

# Make pyramid plot
library(plotrix)

# Words common in both coffee tweets and chardonnay tweets
plotrix::pyramid.plot(
  top25_df$x,
  top25_df$y,
  labels = top25_df$labels,
  main = "Words in Common",
  raxlab = NULL, unit = NULL,
  top.labels = c("Coffee", "Words", "Chardonnay")
)

# Words that are common in chardonnay tweets but rare in coffee tweets
top25_df_uncommon <- all_m %>%
  # Convert to data frame
  as_tibble(rownames = "word") %>%
  # Keep rows where words appear everywhere
  filter_all(all_vars(.>0)) %>%
  # Get difference in counts
  mutate(difference = chardonnay - coffee) %>%
  # Keep rows with biggest difference
  top_n(25, wt = difference) %>%
  # Arrange by descending difference
  arrange(desc(difference))

plotrix::pyramid.plot(
  # Chardonnay counts
  top25_df_uncommon$chardonnay,
  # Coffee counts
  top25_df_uncommon$coffee,
  # Words
  labels = top25_df_uncommon$word,
  top.labels = c("Chardonnay", "Words", "Coffee"),
  main = "Words Common in Chardonnay, but rare in Coffee",
  unit = NULL,
  gap = 8
)
  
# Word Networks in qdap package
word_associate(coffee_tweets$text, 
               match.string = c("barista"),
               stopwords = c(Top100Words, "coffee", "amp"),
               network.plot = TRUE,
               cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Barista Coffee Tweet Association")

word_associate(chardonnay_tweets$text, 
               match.string = c("marvin"), 
               stopwords = c(Top200Words, "chardonnay", "amp"),
               network.plot = TRUE,
               cloud.colors = c("gray85", "darkred"))
title(main = "Chardonnay Tweets Associated with Marvin")

# Simple Word Clustering
# Moves beyond simple one-word frequency to give you exposure to slightly more technical text mining
# Hierarchical clustering example
rain <- data.frame(
  city = c("Cleveland", "Portland", "Boston", "New Orleans"),
  annual_rainfall =  c(39.14, 39.14, 43.77, 62.45)
)

# Distance matrix
dist_rain <- dist(rain$annual_rainfall)

# Convert to hierarchical cluster object
hc <- hclust(dist_rain)

# Plot dendrogram with city labels
plot(hc, labels = rain$city)

# It should be noted that a denrogram will reduce information. If Cleveland and Portland were separated by a small amount, even a single inch, the dendrogram would look the same. 

# Make a dendrogram friendly TDM
# First, create a chardonnay TDM
chardonnay_source <- tm::VectorSource(chardonnay_tweets$text)
chardonnay_corpus <- tm::VCorpus(chardonnay_source)
chardonnay_clean <- clean_corpus(chardonnay_corpus)
tweets_tdm <- TermDocumentMatrix(chardonnay_clean)

dim(tweets_tdm)

# First, we have to limit the number of words in TDM using removeSparseTerms() from tm package.
# TDMs and DTMs are sparse, meaning they contain mostly zeros. Remember that 1000 tweets can become a TDM with over 3000 terms! You won't be able to easily interpret a dendrogram that is so cluttered, especially if you are working on more text.
tweets_tdm

# In most professional settings, a good dendrogram is based on a TDM with 25 to 70 terms. Having more than 70 terms may mean the visual will be cluttered and incomprehensible. Conversely, having less than 25 terms likely means your dendrogram may not plot relevant and insightful clusters. 

# When using removeSparseTerms(), the sparse parameter will adjust the total terms kept in the TDM. The closer sparse is to 1; the more terms are kept. This value represents a percentage cutoff of zeros for each term in the TDM.

# Create tdm1
tdm1 <- tm::removeSparseTerms(tweets_tdm, sparse = 0.98)

# Create tdm2
tdm2 <- tm::removeSparseTerms(tweets_tdm, sparse = 0.985)

# You have to convert tdm into matrix before using them with dist() function
tdm_m <- as.matrix(tdm2)

# Create tweets_dist
tweets_dist <- dist(tdm_m)

hc <- hclust(tweets_dist)
plot(hc)

# Dendrogram aesthetics
library(dendextend)
# The dendextend package can help your audience by coloring branches and outlining clusters. dendextend is designed to operate on dendrogram objects, so you'll have to change the hierarchical cluster from hclust using as.dendrogram().

# A good way to review the terms in your dendrogram is with the labels() function. It will print all terms of the dendrogram. To highlight specific branches, use branches_attr_by_labels(). First, pass in the dendrogram object, then a vector of terms as in c("data", "camp"). Lastly, add a color such as "blue".

# After you make your plot, you can call out clusters with rect.dendrogram(). This adds rectangles for each cluster. The first argument to rect.dendrogram() is the dendrogram, followed by the number of clusters (k). You can also pass a border argument specifying what color you want the rectangles to be (e.g. "green").

# Create dendrogram object
hcd <- as.dendrogram(hc)

# Print the labels in hc
labels(hcd)

# Change the branch color to red for "marvin" and "gaye"
hcd_colored <- branches_attr_by_labels(hcd, 
                                       labels = c("marvin", "gaye"),
                                       color = "red")

# Plot hcd_colored
plot(hcd_colored, main = "Better Dendrogram")

# Add cluster rectangles
rect.dendrogram(hcd_colored, k = 2, border = "grey50")

# Using word association
# Another way to think about word relationships is with the findAssocs() function in the tm package. For any given word, findAssocs() calculates its correlation with every other word in a TDM or DTM. Scores range from 0 to 1. A score of 1 means that two words always appear together in documents, while a score approaching 0 means the terms seldom appear in the same document. 

# Keep in mind the calculation for findAssocs() is done at the document level. So for every document that contains the word in question, the other terms in those specific documents are associated. Documents without the search term are ignored. 

# To use findAssocs() pass in a TDM or DTM, the search term, and a minimum correlation. The function will return a list of all other terms that meet or exceed the minimum threshold.

# findAssocs(tdm, "word", 0.25)

# Minimum correlation values are often relatively low because of word diversity. Don't be surprised if 0.10 demonstrates a strong pairwise term association. 

coffee_tweets_source <- tm::VectorSource(coffee_tweets$text)
coffee_tweets_corpus <- tm::VCorpus(coffee_tweets_source)
coffee_corpus_clean <- clean_corpus(coffee_tweets_corpus)
coffee_tweets_tdm <- TermDocumentMatrix(coffee_corpus_clean)

coffee_tweets_tdm

dim(coffee_tweets_tdm)

# Create associations
associations <- findAssocs(coffee_tweets_tdm, "venti", 0.2)

# View the venti associations
associations
class(associations)

# Create association_df using list_vect2df in qdapTools package
# Convert a list of named vectors to a hierarchical dataframe
associations_df <- list_vect2df(associations, 
                                col2 = "word",
                                col3 = "score")

# Plot the associations_df values
associations_df %>%
  ggplot(aes(x = score, y = word))+
  geom_point(size = 3)+
  theme_gdocs()

# Getting past single words
# So far only worked with single words, we can change the tokenization of our terms
# This is important because "not" and "good" as separate words have a very different meaning compared to the bigram "not good". Increasing the tokenization will increase the DTM and TDM size.

# Use only first 2 coffee tweets
coffee_tweets$text[1:2]

# Make a unigram DTM on first 2 coffee tweets
text_data <- tm::VectorSource(coffee_tweets$text[1:2])
text_corp <- tm::VCorpus(text_data)
text_corp_clean <- clean_corpus(text_corp)
unigram_dtm <- DocumentTermMatrix(text_corp_clean)
unigram_dtm

# Change from unigram to bigram tokenization
library(RWeka)

# Define bigram tokenizer
tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

# Make a bigram TDM
bigram_tdm <- TermDocumentMatrix(
  text_corp_clean,
  control = list(tokenize = tokenizer)
)

bigram_tdm

# How do bigrams affect word clouds?
bigram_dtm <- DocumentTermMatrix(
  chardonnay_clean, 
  control = list(tokenize = tokenizer)
)

# Create a bigram DTM Matrix
bigram_dtm_m <- as.matrix(bigram_dtm)

# Create freq
freq <- Matrix::colSums(bigram_dtm_m)

# Create bi words vector
bi_words <- names(freq)

# Review all bigrams starting with "marvin"
library(rebus)
str_subset(bi_words, pattern = START %R% "marvin")

# Plot a Word Cloud
wordcloud(words = bi_words,
          freq = freq, 
          max.words = 15,
          colors = "blue")

# Different Frequency Criteria
# Term weights
# Default term frequency = simple word count
# Frequent words can mask insights
# To combat this when constructing a TDM, you can pass in a control parameter to change the term weighting. 
# Words appearing in many documents are penalized
# The thought being that words that are both common and across all documents have little informational value.

clean_corpus <- function(corpus){
  # Remove white spaces
  corpus <- tm::tm_map(corpus, stripWhitespace)
  # Remove numbers
  corpus <- tm::tm_map(corpus, removeNumbers)
  # Replace contractions
  corpus <- tm::tm_map(corpus, content_transformer(replace_contraction))
  # Transform to lower case
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  # Remove some words
  corpus <- tm::tm_map(corpus, removeWords, c(stopwords("en"), 
                                              "amp"))
  # Remove Punctuation
  corpus <- tm::tm_map(corpus, removePunctuation)
  return(corpus)
}

# Example
# Standard term weighting 
coffee_text <- tm::VectorSource(coffee_tweets$text)
text_corp <- tm::VCorpus(coffee_text)
tf_tdm <- TermDocumentMatrix(clean_corpus(text_corp))
tf_tdm_m <- as.matrix(tf_tdm)
tf_tdm_m[471:477, 5:10]

# TfIdf weighting
tf_idf_tdm <- TermDocumentMatrix(clean_corpus(text_corp), 
                                 control = list(weighting = weightTfIdf))
tf_idf_tdm_m <- as.matrix(tf_idf_tdm)
tf_idf_tdm_m[471:477, 5:10]
# lower the impact of coffee

# Retaining document metadata
# Create mapping to metadata

coffee_tweets2 <- coffee_tweets %>%
  dplyr::select(num, text, screenName, created) %>%
  rename("doc_id" = num,  # Should change column name to doc_id
         "author" = screenName, 
         "date" = created)

# Create VCorpus including metadata
test_corpus <- tm::VCorpus(DataframeSource(coffee_tweets2))

# Clean and view results
text_corpus <- clean_corpus(test_corpus)

content(text_corpus[[1]])

# Access Metadata
meta(text_corpus[1])










