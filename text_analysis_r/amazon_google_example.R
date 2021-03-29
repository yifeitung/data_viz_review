library(tidyverse)
library(tm)
library(qdap)
library(wordcloud)
library(Matrix)
library(plotrix)

# A case study in HR analytics

# Help your recruiters understand how external candidates may perceive your organization compared to a competitor. 

# Amazon and Google employee reviews
# Manually gathered 2000 anonymous online reviews from current and former employees from both companies. Performing a sound text mining project usually requires following 6 steps in the context of this workflow. 

# Problem: Which company has better work life balance? Which has better perceived pay according to online reviews?

# Identifying the text sources
amzn <- read.csv("dataset/500_amzn.csv", stringsAsFactors = FALSE)
amzn <- amzn[complete.cases(amzn), ]
str(amzn)
goog <- read.csv("dataset/500_goog.csv", stringsAsFactors = FALSE)
goog <- goog[complete.cases(goog), ]
str(goog)

# Create amzn_pros
amzn_pros <- amzn$pros

# Create amzn_cos
amzn_cons <- amzn$cons

# Create goog_pros
goog_pros <- goog$pros

# Create goog_cons
goog_cons <- goog$cons

# Text Organization

# qdap cleaning function
# qdap function can be applied directly to a text vector, rather than a corpus object. 
qdap_clean <- function(x){
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}

# tm cleaning function
tm_clean <- function(corpus){
  corpus <- tm::tm_map(corpus, removePunctuation)
  corpus <- tm::tm_map(corpus, stripWhitespace)
  corpus <- tm::tm_map(corpus, removeWords, 
                       c(stopwords("en"), "Google", "Amazon", "company"))
  return(corpus)
}

# Amazon Pros
# qdap_clean the text
qdap_cleaned_amzn_pros <- qdap_clean(amzn_pros)

# Source and create the corpus
amzn_p_corp <- tm::VCorpus(VectorSource(qdap_cleaned_amzn_pros))

# tm_clean the corpus
amzn_pros_corp <- tm_clean(amzn_p_corp)

# Amazon Cons
# qdap_clean the text
qdap_cleaned_amzn_cons <- qdap_clean(amzn_cons)

# Source and create the corpus
qdap_c_corp <- VCorpus(VectorSource(qdap_cleaned_amzn_cons))

# tm_clean the corpus
amzn_cons_corp <- tm_clean(qdap_c_corp)

# Google Pros
# qdap_clean the text
qdap_cleaned_goog_pros <- qdap_clean(goog_pros)

# Source and create the corpus
goog_p_corp <- VCorpus(VectorSource(qdap_cleaned_goog_pros))

# tm_clean the corpus
goog_pros_corp <- tm_clean(goog_p_corp)

# qdap clean the text
qdap_cleaned_goog_cons <- qdap_clean(goog_cons)

# Source and create the corpus
goog_c_corp <- VCorpus(VectorSource(qdap_cleaned_goog_cons))

# tm clean the corpus
goog_cons_corp <- tm_clean(goog_c_corp)

# Step 4 & 5: Feature extraction & Analysis
library(RWeka)

# # Define bigram tokenizer
tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

# Create amzn_p_tdm
amzn_p_tdm <- TermDocumentMatrix(
  amzn_pros_corp,
  control = list(tokenize = tokenizer)
)

# Create amzn_p_tdm_m
amzn_p_tdm_m <- as.matrix(amzn_p_tdm)

# Create amzn_p_freq
amzn_p_freq <- Matrix::rowSums(amzn_p_tdm_m)

# Plot a word cloud using amzn_p_freq values
wordcloud(words = names(amzn_p_freq), freq = amzn_p_freq, 
          max.words = 25, 
          scale = c(2, 0.8),
          color = "blue")

# Create amzn_c_tdm
amzn_c_tdm <- TermDocumentMatrix(
  amzn_cons_corp,
  control = list(tokenize = tokenizer)
)

# Create amzn_c_tdm_m
amzn_c_tdm_m <- as.matrix(amzn_c_tdm)

# Create amzn_c_freq
amzn_c_freq <- Matrix::rowSums(amzn_c_tdm_m)

# Plot a word cloud of negative Amazon bigrams
wordcloud(words = names(amzn_c_freq), freq = amzn_c_freq, 
          scales = c(2, 0.5),
          max.words = 25, 
          color = "red")

# Amazon Cons Dendrogram
# Create amzn_c_tdm2 by removing sparse terms 
amzn_c_tdm2 <- removeSparseTerms(amzn_c_tdm, sparse = 0.993)

# Create hc as a cluster of distance values
hc <- hclust(dist(amzn_c_tdm2),
             method = "complete")

# Produce a plot of hc
plot(hc)

# Word Association
# Create term_frequency
term_frequency <- sort(amzn_p_freq, decreasing = TRUE)

# Print the 5 most common terms
term_frequency[1:5]

# Look at the terms most associated with "fast paced", it could be a negative term related to "long hours"
findAssocs(amzn_p_tdm, "fast paced", 0.2)

# Create a comparison cloud of Google's positive and negative reviews for comparison to Amazon.  This will give you a quick understanding of top terms without having to spend as much time as you did, examining the Amazon reviews in the previous exercises.

google_pos <- paste(goog$pros, collapse = " ")
google_cos <- paste(goog$cons, collapse = " ")
all_goog <- c(google_pos, google_cos)
all_goog <- qdap_clean(all_goog)
all_goog_source <- tm::VectorSource(all_goog)
all_goog_corpus <- tm::VCorpus(all_goog_source)
all_goog_corpus <- tm_clean(all_goog_corpus)

all_goog_corp <- TermDocumentMatrix(all_goog_corpus)
colnames(all_goog_corp) <- c("Pros", "Cons")

all_m <- as.matrix(all_goog_corp)

comparison.cloud(all_m, 
                 max.words = 100, 
                 colors = c("#F44336", "#2196f3"), 
                 random.order = FALSE)

# Amazon vs. Google Pro Reviews
# Filter to words in common and create an absolute diff column
amzn_pros <- paste(amzn$pros, collapse = " ")
google_pos <- paste(goog$pros, collapse = " ")
all_pros <- c(amzn_pros, google_pos)
all_pros <- qdap_clean(all_pros)
all_pros_source <- VectorSource(all_pros)
all_pros_corps <- VCorpus(all_pros_source)
all_tdm <- TermDocumentMatrix(tm_clean(all_pros_corps), 
                              control = list(tokenize = tokenizer))

all_tdm_m <- as.matrix(all_tdm)
colnames(all_tdm_m) <- c("AmazonPro", "GooglePro")
all_tdm_df <- all_tdm_m %>%
  as_tibble(rownames = "terms") 

common_words <- all_tdm_df %>%
  filter(AmazonPro != 0 & GooglePro != 0) %>%
  mutate(diff = abs(AmazonPro - GooglePro))

# Extract top 5 common bigrams
top10_df <- common_words %>%
  arrange(desc(diff)) %>%
  top_n(10, wt = diff)

# Create the pyramid plot
pyramid.plot(top10_df$AmazonPro, top10_df$GooglePro, 
             labels = top10_df$terms, gap = 12, 
             top.labels = c("Amzn", "Pro Words", "Goog"), 
             main = "Words in Common", unit = NULL)

# Step 6: Reach a conclusion
findAssocs(amzn_p_tdm, "fast paced", 0.2)[[1]][1:15]

# Identify candidates that view an intense workload as an opportunity to learn fast and give them ample opportunities.

# Summarize Skills
# Organize and clean text data
# Tokenize into unigrams & bigrams
# Build TDMs & DTMs
# Extract Features
# 1. Top terms
# 2. Word associations
# Visualize text data

