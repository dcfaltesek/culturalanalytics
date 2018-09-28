#this is our base code for day four - where we learn about scrapers

#this is our featured library
library(rvest)

#it would be important to have these running as well
library(ggplot2)
library(dplyr)
library(tibble)
#LETS BE HONEST - YOU WILL NEED A TON OF LIBRARIES. 

#rvest is popular and effective, it allows a number of options for quick scraping 

#first: store the location that will be scraped
feetball<-"https://www.sports-reference.com/cfb/years/2018-schedule.html"

foot<-feetball%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_nodes(xpath='//*[@id="schedule"]')%>%
  html_table()%T>%
  View()

football<-as.tibble(data.frame(foot))
View(football)

#Let's table the football question for now, we will wrangle this in a few dozen lines

#movie example
#older tutorials use "read" now we have read_html
lebowski <- read_html("https://www.imdb.com/title/tt0118715/")


#check out lebowski in your global environment: it did not simply store the URL but it actually hit the website it
#there are two sub-lists: the head and the body

#this code then is reaching into the stored region and basically running a specialized search/find operation 
leb_cast<-lebowski %>%
  #XPATH IS INSIDE OF SINGLE MARK
  html_nodes(xpath = '//*[@id="titleCast"]/table') %>%
  html_table(header = TRUE)

as.tibble(data.frame(leb_cast))

#and if we want the full cast
L2<-read_html("https://www.imdb.com/title/tt0118715/fullcredits?ref_=tt_cl_sm#cast")

L2%>%
  #KEY THING - most xpaths that are useful for tables end with /table
  html_nodes(xpath = '//*[@id="fullcredits_content"]/table')%>%
  html_table(header=TRUE)

#perhaps we just want the place where the movie poster is
poster <- lebowski %>%
  html_nodes(".poster img") %>%
  html_attr("src")

#you can then 
download.file(poster, "poster.png")
#sweet, I had this poter in my college dorm


#more practice and an old school feature from reshape2
elect<-read_html("https://en.wikipedia.org/wiki/Electoral_College_(United_States)")

elect2<-elect%>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[7]')%>%
  html_table(fill = TRUE)

elect3<-as_tibble(data.frame(elect2))
View(elect3)

elect4<-slice(elect3, 4:54)

#clean this now. I mean, this is super yucky. 
elect5<-select(elect4, -Electionyear)


elect5%>%
  filter(Electionyear.1 == "California")%>%
  ggplot(aes(variable, value))+geom_point()

#Let's get some text shall we
#here is an article about steak

steak<-read_html("https://thetakeout.com/drew-magary-ruins-wagyu-steak-1827996474")

steak%>%
  html_nodes("p")%>%
  html_text()%T>%
  View()

sprouts<-read_html("https://www.bonappetit.com/recipe/roasted-brussels-sprouts-with-warm-honey-glaze")
sprouts%>%
  html_nodes(".ingredient")%>%
  html_text()

sprouts%>%
  html_nodes("#react-app > div > div.post-layout > div.grid.grid--post-layout > div > div > div.col.col-xs-4.col-sm-10.col-md-8.col--MainWithSidebar.col--main-content > div.content-group > div.row.steps-area > div > ul")%>%
  html_text()

#a quick dirty method to do a lot of this ... a better method would be to write a function with a for-loop, but that is in a few weeks

#check out this URL
"http://archive.fortune.com/magazines/fortune/fortune500_archive/full/1955/"

#now this one
"http://archive.fortune.com/magazines/fortune/fortune500_archive/full/1990/"

#what do you notice? 
#They did a really good job with consistent URLs
#you can do this in excel if you need to, and honestly the correct method is a for-loop, but this is just such a great MacGuvyer

#THINGS TO KNOW adding quotes to things is really tricky. 


fLines<-data.frame(A="fortune", B=1955:2010, C="<-read_html",D="(", E="http://archive.fortune.com/magazines/fortune/fortune500_archive/full/", F=1955:2010, G=")", stringsAsFactors = FALSE)
View(fLines)

FFlines<-paste(fLines$A, fLines$B, fLines$C, fLines$D, "'", fLines$E, fLines$F, "'", fLines$G, sep="")
Alright<-data.frame(FFlines, stringsAsFactors = FALSE)

#LULZ! THE POWER!!!!!!!!!
eval(parse(text = Alright$FFlines))

#a 404 error - check your global environment...


fortune2002%>%
  #KEY THING - most xpaths that are useful for tables end with /table
  html_nodes(xpath = '//*[@id="MagListDataTable"]/table[2]')%>%
  html_table(header=TRUE)

cyclops<-paste(fPop$A, fPop$B, "B", sep="")
gambit<-paste(fPop$A, fPop$B, sep="")

Wolverine<-paste(cyclops, "<-", gambit, "%>%",c("html_nodes(xpath=\'//*[@id=\"MagListDataTable\"]/table[2]\')%>%html_table(header=TRUE)"))
writeLines(Wolverine)

#copy and run one of those lines from wolverine
#even moar power, and since we stored in vars, it's like hello

eval(parse(text = Wolverine))

#the next step
rogue<-paste(cyclops, ",", sep="")
writeLines(rogue)

storm<-paste(cyclops,"<-","data.frame(",cyclops,")", sep = "")
writeLines(storm)
eval(parse(text = storm))

Nightcrawler<-paste(cyclops,"<-mutate(", cyclops, ",", "year=", fPop$B, ")", sep="")
writeLines(Nightcrawler)
eval(parse(text = Nightcrawler))

#lets do this the wrong way. Copy that output from the console
Magneto<-rbind(fortune1955B,
          fortune1956B,
          fortune1957B,
          fortune1958B,
          fortune1959B,
          fortune1960B,
          fortune1961B,
          fortune1962B,
          fortune1963B,
          fortune1964B,
          fortune1965B,
          fortune1966B,
          fortune1967B,
          fortune1968B,
          fortune1969B,
          fortune1970B,
          fortune1971B,
          fortune1972B,
          fortune1973B,
          fortune1974B,
          fortune1975B,
          fortune1976B,
          fortune1977B,
          fortune1978B,
          fortune1979B,
          fortune1980B,
          fortune1981B,
          fortune1982B,
          fortune1983B,
          fortune1984B,
          fortune1985B,
          fortune1986B,
          fortune1987B,
          fortune1988B,
          fortune1989B,
          fortune1990B,
          fortune1991B,
          fortune1992B,
          fortune1993B,
          fortune1994B,
          fortune1995B,
          fortune1996B,
          fortune1997B,
          fortune1998B,
          fortune1999B,
          fortune2000B,
          fortune2001B,
          fortune2002B,
          fortune2003B,
          fortune2004B,
          fortune2005B)%T>%
  View()
#now go ahead and rename those columns
distinct(Magneto, Company)
