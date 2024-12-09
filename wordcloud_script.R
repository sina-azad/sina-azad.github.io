# wordcloud

# install and load packages
pacman::p_load(rio, tidyverse, tm, wordcloud, RColorBrewer)

# data
data <- import("pubs_list.csv")
#glimpse(data)


# Combine the "title" and "journal" columns into one text vector
text_data <- paste(data$title, data$journal, collapse = " ")


# Create a corpus
corpus <- Corpus(VectorSource(text_data))


# Clean the text: convert to lower case, remove punctuation, remove numbers, and common stopwords
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))


# Create a term-document matrix + Extract word frequencies
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freq <- sort(rowSums(m), decreasing = TRUE)

#head(word_freq)

# Create a data frame of words and frequencies
word_data <- data.frame(word = names(word_freq), freq = word_freq)


# Plot the word cloud using wordcloud
set.seed(123)  # For reproducibility

wordcloud(words = word_data$word,
          freq = word_data$freq,
          min.freq = 2,   
          max.words = 200,
          random.order = FALSE,
          random.color = TRUE, 
          scale = c(3.5, 1), # range of the size of the words
          rot.per = 0.35, # proportion words with 90 degree rotation
          colors = brewer.pal(n = 50, "Dark2")
)


