library(tidytext)
library(textfeatures)

library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)

#core code in this initial round insipired by Mike Kearney

sneaker <- read_html("https://www.nytimes.com/2018/10/18/fashion/peak-sneaker-fashion.html")

sneaker_text <- sneaker %>%
  html_nodes("p")%>%
  html_text

as_tibble(sneaker_text)
View(sneaker_text)
sneaker <- data.frame(sneaker_text, stringsAsFactors = FALSE)
head(sneaker)

#destructive pull advertisement
sneaker<-filter(sneaker, sneaker_text != "Advertisement")
colnames(sneaker)[1]<-"text"

tf_sneaker<-textfeatures(sneaker)
as_tibble(tf_sneaker)
#interpretation of the data

SNEAKER<-bind_cols(sneaker, tf_sneaker)

View(SNEAKER)

b <- 1:37
b<-data_frame(b)
SNEAKER<-bind_cols(SNEAKER, b)
dim(SNEAKER)
colnames(SNEAKER)[19]<-"paragraph"

View(SNEAKER)
ggplot(SNEAKER, aes(paragraph, n_chars, colour = n_periods))+geom_jitter()

#how many periods per sentence...
mean(SNEAKER$n_periods)

#LETS REFACTOR THIS INTO A SMALL PROGRAM


#NOW let's turn to the tidytext methods

#turns things into individual words
library(janeaustenr)
#this pacakge is mainly just the books
??janeaustenr
View(austen_books)

#what did we learn?
#it's a function that calls a list.

#a recommended call from the documentation 
austen_books() %>% group_by(book) %>%
  summarise(total_lines = n())

#let's take a closer look at Sense and Sensibility
View(sensesensibility)#WHAT STRUCTURE, SUCH ELEGANCE DATABASE

#lincoln
g_works <- gutenberg_works(author == "Lincoln, Abraham")
g_works%>% count(title)

View(g_works %>% gutenberg_download(meta_fields ="title"))

Z<-gutenberg_download(4)
