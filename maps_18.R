#WARNING! - HALF OF THIS CODE REQUIRES A GOOGLE CLOUD ACCOUNT WITH A CREDIT CARD 
#WE SHOULD ONLY TAKE A MAP QUERY OCCASSIONALLY, THEN BUILD WITH IT.


#load these libraries, you may need to download maps and knitr
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(knitr)
library(ggthemes)

#very large collection of maps, including US counties and lakes, great package
library(maps)

#additional map data
library(mapdata)

#how we will take this next level
library(ggmap)
#another great library from the makers of ggvis, ggplot, rvest, dpylr and many others

#Load the state map
states <- map_data("state")
head(states)
#what do you see?
#once you load the data as a variable, modifying it is straight forward


#before we modify the data, let's try a few quick maps to get a hang of how the code works
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

#you should now have a beautiful map

#if you want to map a handful of states or a region

#create a region
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

#graph it
ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)


#we have now demonstrated basic mapping technique. 
#there is a dataset called elect4 on canvas

#now we should build a dataset that can use this technique
head(elect4)
head(states)

#what do you notice? Is there a common variable name? Is the capitalization correct?

elector <- inner_join(states, elect4, by = "region")
tbl_df(elector)

#now, lets try to sub-in our new data
ggplot(data = elector) + 
  geom_polygon(aes(x = long, y = lat, fill = Status, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

#did that make a pretty picture? 

#what is missing? A key. So what do we do with that? Remove the last line.
ggplot(data = elector) + 
  geom_polygon(aes(x = long, y = lat, fill = Status, group = group), color = "white") + 
  coord_fixed(1.3)

#that's better?

#let's take our aesthetics to the next level. 

#store the map you want to draw as a variable, in this case, use d
d<-ggplot(data = elector) + geom_polygon(aes(x = long, y = lat, group = group, fill=elector$Status)) + coord_fixed(1.3)

#oh and for fun
d+coord_polar()

#this didn't produce a map, instead it simply stored your prefered map, which is useful, if you want to modify said map

#try this - takes out the peksy lines
ggplot(data = elector) + geom_polygon(aes(x = long, y = lat, group = group, fill=elector$Status)) + scale_fill_brewer()+coord_fixed(1.3)+theme_economist()

#prettier colors
ggplot(elector, aes(x=long, y=lat, group=group, fill=as.factor(Value)))+ 
  scale_fill_brewer()+
  geom_polygon()+coord_map()+
  labs(fill="type of connection between voters and electors", title="electoral college, 2016",x="",y="")+theme_economist()



#with controls for the gradients
ggplot(elector, aes(x=long, y=lat, group=group, fill=Value))+ 
  scale_fill_gradient(low="red",high="blue")+
  geom_polygon()+coord_map()+
  labs(fill="type of connection between voters and electors", title="electoral college, 2016",x="",y="")+theme_bw()

#if you are interested, this is the website with the colors named in r
http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

#why not salmon?
ggplot(elector, aes(x=long, y=lat, group=group, fill=Value))+ 
  scale_fill_gradient(low="lightsalmon1",high="lightsalmon4")+
  geom_polygon()+coord_map()+
  labs(fill="type of connection between voters and electors", title="electoral college, 2016",x="",y="")+theme_bw()


#if you can structure a dataset to look like elect4; you can follow these instructions to produce new maps

#when you get to this point, stop and wait.

#you will need a recent version of ggmap
#WAIT WAIT WAIT STOP STOP STOP

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)

#now the key is this
??register_google

#signin to google - KEEP YOUR API KEY SECRET 
register_google(key = "YOUR_KEY_TEXT_HERE")

#your current credentials
ggmap_credentials()

#store a location as a string
osu <- "Oregon State University"

#now ask google to get me a map of that string at zoom 12
qmap(osu, zoom = 12)

#slowing this down
#get a geocode
a<-geocode("Univeristy of Hawaii")
a
#stored as a lon ~ lat

#longitude is X, latitude is Y 
#people weirdly invert x and y in cartographic thinking

#get the map for that location
b<-get_map(location=a)

#plot that map
ggmap(b)

#ontology: ggmap is passing a lon~lat pair to google, which then passes an image back

#Let's compare using UW
c <- geocode("university of washington, seattle")
d<-get_map(c)
ggmap(d)

#you can also use the map_distance api
from<-"seattle,washington"
to<-"corvallis,oregon"

#pulling all
mapdist(from, to)

#adding more facets - MODE IS NOW A SINGLE ARGUMENT - the other specifications here seem to be null now
mapdist(from, to, mode = "walking")
#you can also choose driving, walkingtransit

#what if I want a bunch of addresses
df <- data.frame(
  address = c("1600 Pennsylvania Avenue, Washington DC", "", "waco texas"),
  stringsAsFactors = FALSE
)
df %>% mutate_geocode(address)

#what about routes
trek_df <- trek("corvallis, oregon", "seattle, washington", structure = "route")
#  Source : https://maps.googleapis.com/maps/api/directions/json?origin=houson%2C%20texas&destination=waco%2C%20texas&mode=driving&units=metric&alternatives=false

qmap("portland, oregon", zoom = 6) +
  geom_path(
    aes(x = lon, y = lat),  colour = "blue",
    size = 1.5, alpha = .5,
    data = trek_df, lineend = "round"
  )


#you have seen forward geocode, what about reverse (it doesn't do a good job with oceans)
revgeocode(c(lon = 107.1161, lat = 31.55098))

#many types of maps are avaialble as well _ RUN ONE AT A TIME
get_googlemap("corvallis, oregon", zoom = 12, maptype = "satellite") %>% ggmap()
get_googlemap("corvallis, oregon", zoom = 12, maptype = "terrain") %>% ggmap()
get_googlemap("corvallis, oregon", zoom = 12, maptype = "roadmap") %>% ggmap()
get_googlemap("corvallis, oregon", zoom = 12, maptype = "hybrid") %>% ggmap()+theme_tufte()


#notice in stamenmap you must declare the edges of the map
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "watercolor")
ggmap(map)

#possible maptypes: terrain, terrrain-background, terrain-labels, terrain-lines, toner, toner-2010, toner-2011, toner-background, toner-hybrid, toner-labels, toner-lines, toner-lite, watercolor
map <- get_stamenmap(us, zoom = 5, maptype = "terrain-lines")

#texas
map <- get_map(location = "texas", zoom = 6)
ggmap(map)

#let's look at some other map alternatives
??get_map

#load the donut dataset

#a sequence on portland
port<-geocode("portland, oregon")
portland<-get_map(port)
PDX<-ggmap(portland)
ggmap(portland)

#a nice little mappy
PDX +
  geom_point(aes(x = lon, y = lat, colour = hip, size = flavor,stroke =5), data = donut)

#let's zoom this up a little
#call the map from google
portland<-get_map(port, zoom = 11)

#now we translate the basic plotting object into an intermediate var
PDX<-ggmap(portland)
PDX +
  geom_point(aes(x = lon, y = lat, colour = hip, size = flavor,stroke =5), data = donut)

#now a quick jitter
PDX +
  geom_jitter(aes(x = lon, y = lat, colour = hip, size = flavor), data = donut)

#with geom_text
PDX + 
  geom_text(aes(x = lon, y = lat, colour = hip, size = flavor, label = name), data = donut)

#and crime in Corvallis via Zac
#import corvallis noise and let's start cleaning
library(lubridate)
#parsed times
bonkers<-mdy_hms(corvallis_noise$DATE)
hour<-hour(bonkers)
month_day<-day(bonkers)
month<-month(bonkers)
year<-year(bonkers)
bh<-data.frame(hour, month_day, month, year)
noise<-bind_cols(corvallis_noise, bh)
View(noise)

#NOW we must be frugal
noiseA<-filter(noise, year > 2017)%>%
  filter(month > 8)

#make a dataframe with the street_addresses
df <- data.frame(
  street_address = noise$street_address,
  stringsAsFactors = FALSE
)

#mutate_geocode to put that list into a new dataframe
locations<-df %>% mutate_geocode(street_address)

#inner_join that to the original
noise2 <- inner_join(noise, locations, by = "street_address")

#take a quick look
View(noise2)

#get a road map of town
corvallis<-get_googlemap("corvallis, oregon", zoom = 14, maptype = "road")
corvallis2<-ggmap(corvallis)

#and the fun begins...
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = type), data = noise2)

#and the police really helped us here with formatting
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = type), data = noise2)+facet_wrap(. ~day)

#from the Houston crimes example
corvallis2 +
  stat_bin2d(
    aes(x = lon, y = lat, colour = type, fill = type),
    size = .5, bins = 30, alpha = 1/2,
    data = noise2
  )


