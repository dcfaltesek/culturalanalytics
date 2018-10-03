#code that will parse current football data as a social network
library(magrittr)
library(stringr)

#get the data
feetball<-"https://www.sports-reference.com/cfb/years/2018-schedule.html"

#parse the football data
foot<-feetball%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_nodes(xpath='//*[@id="schedule"]')%>%
  html_table(header = TRUE)

#make the data tidy
foot<-as_tibble(data.frame(foot))
View(foot)

#filter for games that have actually happened. 
filter(foot, Pts>0)
#structure data 
football_data<-rename(games, "SOURCE" = "Loser")%>%
  rename("TARGET" = "Winner")%>%
  select(SOURCE, TARGET)

#now cleaning
A<-str_replace_all(football_data$SOURCE, "\\(", "")
B<-str_replace_all(A, "\\)", "")
C<-str_replace_all(B, "[:digit:]", "")
D<-str_trim(C, side="left")

G<-str_replace_all(football_data$TARGET, "\\(", "")
H<-str_replace_all(G, "\\)", "")
I<-str_replace_all(H, "[:digit:]", "")
J<-str_trim(I, side="left")



#combine and rename
clean_football<-as_tibble(data.frame(D, J))
clean_football<-rename(clean_football, "SOURCE" = "D")%>%
  rename("TARGET" = "J")

View(clean_football)
#grab all but the rows that include "loser"

clean_football<-filter(clean_football, SOURCE != "Loser")

#now export for gephi
#it is really importat to not write the row names, it really confuses everything in this world
write.csv(clean_football, "clean_football.csv", row.names = FALSE)
