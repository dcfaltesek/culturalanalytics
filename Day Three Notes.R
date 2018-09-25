#day three pre-code examples, to make things run more smoothly

#first, call your libraries for the day
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)
library(magrittr)

#some handydatasets
library(babynames)
library(nycflights13)
library(ggplot2movies)

#that last one is new to you - it is a list of about 60k films from imdb. Let's use that today


#a review of dplyr code: five big functions, this time using 
#filter
filter(movies, Comedy==1)

#so what if we want to see all Comedies after 1990, desending order of length
filter(movies, Comedy==1)%>%
  filter(year>1990)%>%
  arrange(desc(length))

#here is a really funny example
recentComedy<-filter(movies, Comedy==1)%>%
  filter(year>1990)%>%
  #oh and look at that a "T" pipe
  arrange(desc(length))%T>%
  View()
#the T pipe allows you to do something that should have no result but then to add a result
#OK WAIT - GO BACK TO LINE 29 and DELETE the T PIPE! SEE WHAT HAPPENS. 

#You did it? Excellent. It just freaked out because that View needed to grab somethign
#But why would you need the T pipe?
#there are some workflows with d3.js that want to print or store things, %<>% can be used to assign things in pipe system

#Back to the extraction theme
#Let's use babynames this time
#we know that there are nearly two-million entires in this dataset
#but HOW MANY ACTUAL NAMES?

distinct(babynames, name)
#how many...

#Ok, this time, let's really do some science to the NYC airports. Instead of letting them use averages for a whole year, lets get random flights
sample_frac(flights, size = .05, replace = FALSE)

#now lets see how much the average delay was
sample_frac(flights, size = .05)%>%
  summarize(mean(dep_delay, na.rm = TRUE))

#run lines 51 and 52 FIVE TIMES. 
#What do you notice? 

#perhaps you just want some random number of rows
sample_n(babynames, 14)

#if you ever need a random name generator for fiction writing, this is your jam. 

#Let's say you want to arrange all the baby names by popularity ever, and THEN slice off the top 25 names of all time, then to see the distinct count

arrange(babynames, desc(n))%>%
  slice(1:20)%>%
  distinct(name)

#Let's find the height of the age of conformity
arrange(babynames, desc(n))%>%
  slice(1:20)%>%
  summarize(mean(year))

#Well that's it. 
#What if we use the top 100? 

top_n(babynames, 25, n)%>%
  summarize(mean(year))

#a density graphic for the age of conformity 
top_n(babynames, 500, n)%>%
  ggplot(aes(year, n, colour=sex))+geom_density2d()

#really stretch out that plot zoom, you won't be disappointed 

#ok, why not
top_n(babynames, 500, n)%>%
  ggplot(aes(year, n, colour=sex))+geom_hex()

#OH WAIT, didn't work? Trouble shoot this...

#and just for information sake...
top_n(babynames, 500, n)%>%
  ggplot(aes(year, n, colour=sex))+geom_text(aes(label = name), check_overlap = TRUE)

#So what are we really learning about Linda...
filter(babynames, name == "Linda")%>%
  ggplot(aes(year, prop))+geom_area()

#NERD ALERT - time for kurtosis
#this is the measure of how sharp that point is...
#you will need these handy libraries for some real cool maths
library(moments)
library(e1071)

linda<-filter(babynames, name == "Linda")
kurtosis(linda$n)
skewness(linda$n)
#let's do a quick comp

Jen<-filter(babynames, name == "Jennifer")
kurtosis(Jen$n)
skewness(Jen$n)

#for comp sake, 
Rob<-filter(babynames, name == "Robert")
kurtosis(Rob$n)
skewness(Rob$n)

#so which one has a sharper peak?
#Which one has more total children...
sum(linda$n)
sum(Jen$n)

#Professors: what is your conclusion?

#AT THIS POINT RUN THIS HELP CODE.
??base::logic

#You may need to pull stuff out of a column with pull or select
pull(movies, year)

#that's just all the values from that column as a vector, it could be useful

#what if I don't care about genre...
select(movies, title, year, length, rating)
#a nice new dataset without the columns I didn't need

#Let's work with VECTOR summary functions

#get me all flights on Valentine's Day
filter(flights, month==2 & day==14)%>%
  #make a new column called rocker that shows in real tie 
  mutate(rocker = cumsum(dep_delay))%T>%
  #take a T pipe to dump all this data into a CSV, then T pipe again
  write.csv("somedata.csv")%T>%
  #pop it up like a spread sheet and WHILE we are at it, make a GGplot
  View()%>%
  ggplot(aes(dep_time, rocker, colour=carrier))+geom_jitter()

#what even was rocker?
#I would suggest you ??magrittr and read the original description of the code, it is funny and useful
#key syntax for really getting pipes: LHS (left hand side) RHS (right hand side)
#the basic pipe %>% is the most common
#%T>% just returns the LEFT value to pop out of the pipe, sort of like a ninja turtle lifting a lid on a pipe

#the %$% operator carries through the elements of the left side
#this pipe only functions if you call magrittr on your own

#the ggplot2movies dataset from IMDB
movies %>%
  #in this case we want just movies that are above average, but we still want to retain the information about what the ratings were
  subset(rating > mean(rating))%$%
  #since we are now operating in a subset of films and we used the exposition pipe, we can call back to the other parts of the dataset
  cor(rating, length)

#try this code again but with a standard pipe; it couldn't deal with "length" because you needed RIGHT hand side to be able to see the pipe result TOO



#from last time, with a little programming mojo...
filter(babynames, name == "Aubri" | name  == "Aubrey")%>%
  mutate(accurate = if_else(name == "Aubrey", "Wrong", "Right", missing = "WUT"))%>%
  ggplot(aes(year, n, shape = sex, colour = accurate))+geom_point()

#summarize functions are really helpful...

summarize(movies, mean(rating))

babynames%>%
  group_by(year)%>%
  summarize(sum(n), mean(n))

#At this point we need to start talking about putting stuff together

#put two tables next to each other
bind_cols()

#add more rows to a table with the same strucutre
bind_rows()

#set theory operators are a little more complicated, but super rewarding; let's get some data...
#these are called FILTERING JOINS
oldnames<-babynames%>%
  filter(year==1960)%>%
  top_n(100, n)

newnames<-babynames%>%
  filter(year==2015)%>%
  top_n(100, n)

#so now we have oldnames and newnames; each has 100 names.

#Let's see which names have been retained over those 55 years..
semi_join(oldnames, newnames, by = "name")
semi_join(newnames, oldnames, by = "name")

#so names have changed a lot.
#the names that have NOT been retained
anti_join(oldnames, newnames, by = "name")

#let's make this a little easier...

oldboynames<-babynames%>%
  filter(sex == "M")%>%
  filter(year==1960)%>%
  top_n(100, n)

newboynames<-babynames%>%
  filter(sex == "M")%>%
  filter(year==2015)%>%
  top_n(100, n)

semi_join(oldboynames, newboynames, by = "name")

oldgirlnames<-babynames%>%
  filter(sex == "F")%>%
  filter(year==1960)%>%
  top_n(100, n)

newgirlnames<-babynames%>%
  filter(sex == "F")%>%
  filter(year==2015)%>%
  top_n(100, n)

semi_join(oldgirlnames, newgirlnames, by = "name")

#let's try a test, 
reallyoldgirlnames<-babynames%>%
  filter(sex == "F")%>%
  filter(year==1880)%>%
  top_n(100, n)

semi_join(reallyoldgirlnames, newgirlnames, by = "name")
semi_join(reallyoldgirlnames, oldgirlnames, by = "name")

#what can you say about these names then?

#you can also use MUTATING JOINS
#Let's make a version of flights that is pretty easy to read...

concise_flights <- select(flights, origin, dest, dep_delay, arr_delay, carrier)
concise_flights

#but what if you wanted to know the actual names of those carriers? UA is easy, but B6?

#this reveals..
nycflights13::airports
View(airports)

#as you can see, there are five datasets here: airlines, airports, flights, planes, and weather
#lets start with our concise data and add the names of the air carriers

#inner_join allows you to use a "key" value shared between datasets to add more data
inner_join(concise_flights, airlines, by = "carrier")

#Let's assume that JFK has the worst weather and is the norm
weatherB<-filter(weather, origin=="JFK")%>%
  select(-origin)%T>%
  View()

#you are starting to realize that there could be a massive, nascent dataset, all of the data.
allflights<-inner_join(flights, airlines, by = "carrier")%>%
  left_join(planes, by = "tailnum")%>%
  mutate(faa = dest)%>%
  left_join(airports, by = "faa")%>%
  inner_join(weatherB, by = "time_hour")%T>%
  View()

#special problem: three airports
dim(weather)
View(filter(weather, month == 1 & day ==1 & hour ==1))
#here is the problem: if we just innerjoin this, it will add the weather from EACH airport to EVERY flight



#so this is now roughly 1/3 the size. 

wEWR<-weather%>%
  filter(origin == "EWR")
wLGA <- weather%>%
  filter(origin == "LGA")
wJFK <- weather%>%
  filter(origin == "JFK")
tfEWR<-flights%>%
  filter(origin == "EWR")
tfLGA <- flights%>%
  filter(origin == "LGA")
tfJFK <- flights%>%
  filter(origin == "JFK")

LGA<-inner_join(tfLGA, wLGA, by = "time_hour")
EWR<-inner_join(tfEWR, wEWR, by = "time_hour")
JFK<-inner_join(tfJFK, wJFK, by = "time_hour")

#and there you are - intermediate variables to the rescue!
fullFlights<-bind_rows(LGA, EWR, JFK)

#some fun functions
ggplot(fullFlights, aes(time_hour, dep_delay, colour=wind_speed))+geom_jitter()
fullFlights%$%
  cor.test(arr_delay, wind_speed)

#well that doesn't help
fullFlights%$%
  cor.test(dep_delay, wind_dir)

fullFlights%$%
  cor.test(dep_delay, temp)

fullFlights%$%
  cor.test(dep_delay, pressure)


fullFlights%$%
  cor.test(dep_delay, humid)

fullFlights%$%
  cor.test(dep_delay, visib)


