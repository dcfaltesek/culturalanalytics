library(quanteda)

#here are a bunch of speeches
summary(data_corpus_inaugural)

#create a corpus
corpus_kafka <- corpus(Kafka, text_field = "text")

#notice that the texts are concealed 
summary(corpus_kafka)

#create a dfm
c_k<-dfm(corpus_kafka, tolower = TRUE, stem = FALSE, remove_punct = TRUE, remove=stopwords("english"))


#we aren't going to do a lot with tokenized texts here today, we did that 
kafka_tokens<-tokens(corpus_kafka)
tokens_ngrams(kafka_tokens, n = 1:3)

#some measurements of the corpus - many methods
textstat_readability(corpus_kafka, measure = "Strain")

#lexical diversity - again, many options - requries DFM
textstat_lexdiv(c_k, measure = "Maas")

#measure similarity
#their example.
x <- dfm(data_corpus_inaugural,
         tolower = TRUE, stem = FALSE, remove_punct = TRUE,
         remove = stopwords("english"))
#text similarity 
textstat_simil(x, "2017-Trump", method = "cosine")

#keynesss
textstat_keyness(x, target = "2017-Trump")

#CA
textmodel_ca(x, sparse = TRUE, residual_floor = 0.1)

#wordfish
textmodel_wordfish(dfm(data_corpus_irishbudget2010), dir = c(6,5))

#subset a corpus and make a word cloud
data_corpus_inaugural %>% corpus_subset(President == "Obama") %>% dfm(remove = stopwords("english")) %>% textplot_wordcloud()


#for this method you need to create a dfm 
presidential<-dfm(data_corpus_inaugural,
    tolower = TRUE, stem = FALSE, remove_punct = TRUE, remove = stopwords("english"))

textmodel<-textmodel_ca(presidential, sparse = TRUE, residual_floor = 0.1)

#word cloud
data_corpus_inaugural %>% corpus_subset(President == "Obama") %>% dfm(remove = stopwords("english")) %>% textplot_wordcloud()

#xray plot
data_corpus_inaugural %>% corpus_subset(Year > 1945) %>% kwic("american") %>% textplot_xray()

#keyness
data_corpus_inaugural %>% corpus_subset(President %in%
    #declare subset - in the case of the example clinton and trump
    c("Kennedy", "Nixon")) %>% dfm(groups = "President",
    #remove the stop words   
    remove = stopwords("english")) %>% textstat_keyness(target = "Nixon") %>% textplot_keyness()




