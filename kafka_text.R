library(rvest)
library(reshape2)
library(stringr)

kafka_home<-read_html("http://franzkafkastories.com/shortStories.php")

#get all the titles - in this case text a
kafka_titles<-kafka_home%>%
  html_nodes("a")%>%
  html_text

#get all attributes - in this case the values of a
kafka_links<-kafka_home%>%
  html_nodes("a")%>%
  html_attrs

kafka_links_vector<-melt(kafka_links)

#example URL: http://franzkafkastories.com/shortStories.php?story_id=kafka_abent_minded_window_gazing
#slug: http://franzkafkastories.com/shortStories.php?

A <- "read_html("
B <- "\'http://franzkafkastories.com/shortStories.php"
C <- kafka_links_vector$value
D <- "\')"

#create new reference vars to store the documents and pull all the special characters
Z<-str_replace_all(kafka_titles, " ", "")
Z<-str_replace_all(Z, "'", "")
Z<-str_replace_all(Z, "-", "")


#store as an intermediate dataframe just in case and for checking 
kafka_read<-tibble(Z, Y, A, B, C, D)


#use paste to produce actionalbe code
kafka_scraper <-paste(Z,Y,A,B,C,D, sep = "")

#pull off the first three useless lines
writeLines(kafka_scraper)
eval(parse(text = kafka_scraper))

#we have the entire site in memory under var names Z

#how to write a quick interprerer
kafka_fulltext<-paste(Z,"_text<-", Z, "%>%html_nodes(\'p\')%>%html_text", sep="")
writeLines(kafka_fulltext)
eval(parse(text = kafka_fulltext))

#now you have a lot of short stories, let's check a few out
#oh, there is messyness

TheJudgment_text<-str_replace_all(TheJudgment_text, "\n", "")
TheJudgment_text<-str_replace_all(TheJudgment_text, "\t\t\t", "")

#for ease, let's see if we can use single quotes for string replacement
Unhappiness_text<-str_replace_all(Unhappiness_text, '\t\t\t', '')
Unhappiness_text



#we need a vector of dataframes
X <- paste(Z, "_text", sep="")


cleaner_code<-paste(X,"<-str_replace_all(", X, "," , "'\\n'", "," , "''", ")", sep = "")
writeLines(cleaner_code)
eval(parse(text = cleaner_code))

cleaner_code<-paste(X,"<-str_replace_all(", X, "," , "'\\t\\t\\t'", "," , "''", ")", sep = "")
writeLines(cleaner_code)
eval(parse(text = cleaner_code))



#now lets go ahead and pick some datasets
Clothes<-data.frame(Clothes_text)
Clothes<-mutate(Clothes, "paragraph" = 1:4)
Clothes<-mutate(Clothes, "story" = "Clothes")
filter(Clothes, 1:3)

AbsentmindedWindowgazing_text
Rejection_text
Resolutions_text
OntheTram_text
Passersby_text
