#calling mallet from tidy

#why Mallet? It is an old, but solid python library for topic modeling. 
#you will notice that it really does just about one thing and not much more or less

library(mallet)
library(dplyr)
library(tidytext)
library(ggplot2)

data("AssociatedPress", package = "topicmodels")
td <- tidy(AssociatedPress)

# mallet needs a file with stop words
tmp <- tempfile()
writeLines(stop_words$word, tmp)

# THIS IS THE KEY
docs <- td %>%
  group_by(document = as.character(document)) %>%
  summarize(text = paste(rep(term, count), collapse = " "))

docs <- mallet.import(docs$document, docs$text, tmp)

# create and run a topic model
topic_model <- MalletLDA(num.topics = 4)
topic_model$loadDocuments(docs)
topic_model$train(20)

# tidy the word-topic combinations
td_beta <- tidy(topic_model)
td_beta

# Examine the four topics
td_beta %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# find the assignments of each word in each document
assignments <- augment(topic_model, td)
assignments

#my favorite mallet functions
doc.topics<-mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)
topic.words<-mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)

plot(mallet.topic.hclust(doc.topics, topic.words, balance = .4))

#and the topic labels
mallet.topic.labels(topic_model, topic.words, num.top.words = 2)

#and the probability assignments for each document
mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)
