#code to scrape a site like billboard by css rather than xpath
#load standard libraries, also:
library(stringr)
library(dplyr)
library(rvest)
#more on stringr in week 5

#read site
bill<-read_html("https://www.billboard.com/charts/rap-song/1997-01-18")

#get the ranking
B1<-bill%>%
  html_nodes(".chart-list-item__rank")%>%
  html_text()

#artist
B2<-bill%>%
  html_nodes(".chart-list-item__artist")%>%
  html_text()

#song title
B3<-bill%>%
  html_nodes(".chart-list-item__title-text")%>%
  html_text()

#last week
B4<-bill%>%
  html_nodes(".chart-list-item__last-week")%>%
  html_text()

#texts to data.frames
B1<-data.frame(B1)
B2<-data.frame(B2)
B3<-data.frame(B3)
B4<-data.frame(B4)

#clean the columns
B1<-str_replace_all(B1$B1, "\\\n", "")
B2<-str_replace_all(B2$B2, "\\\n", "")
B3<-str_replace_all(B3$B3, "\\\n", "")
B4<-str_replace_all(B4$B4, "\\\n", "")

#data frame it again
B1<-data.frame(B1)
B2<-data.frame(B2)
B3<-data.frame(B3)
B4<-data.frame(B4)

after1<-bind_cols(B1,B2,B3,B4)

#now for the top of the chart.
#using the same inital site
#since we know this is no1
C1<-1
#get the no one artist
C2<-bill%>%
  html_nodes(".chart-number-one__artist")%>%
  html_text()

#get the no 1 title
C3<-bill%>%
  html_nodes(".chart-number-one__title")%>%
  html_text()

#this is where it gets complicated
C4<-bill%>%
  html_nodes(".chart-number-one__last-week")%>%
  html_text()

#from here we must write a conditional mutate to handle such a number.
C5<-data.frame(C4, stringsAsFactors = FALSE)
#clean c2
C2<-data.frame(C2)
C2<-str_replace_all(C2$C2, "\\\n", "")

#make a data frame
C5<-data.frame(C1,C2,C3,C4, stringsAsFactors = FALSE)

#Add the column we want
C5<-mutate(C5, "Last Week" = ifelse(C5$C4>1, "1", "write more code, dan"))
#negative join to clean that
topofchart<-select(C5, -C4)
topofchart<-rename(topofchart, "This Week" = C1)%>%
rename("Artist" = C2)%>%
rename("Title" = C3)
#we now have the top line, lets do the next set

View(after1)
after1<-rename(after1, "This Week" = B1)%>%
  rename("Artist" = B2)%>%
  rename("Title" = B3)%>%
  rename("Last Week" = B4)

#now join both tables 
comp_week<-bind_rows(mutate_all(topofchart, as.character), mutate_all(after1, as.character))

#and mutate to add the week
comp_week<-mutate(comp_week, "Date" = "1-18-1997")

View(comp_week)
