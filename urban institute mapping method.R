#from the thinktank the urban institute
devtools::install_github("UrbanInstitute/urbnmapr")
devtools::install_github("UI-Research/urbnthemes")

#this is the urban institute mapping code: https://github.com/UrbanInstitute


library(urbnmapr)
library(urbnthemes)
library(ggplot2)
library(dplyr)
#two tibbles: states and counties

#select states
states %>%
  #classic cartography command
  ggplot(aes(long, lat, group = group)) +
  #polygons
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

territories_counties <- get_urbn_map(map = "territories_counties")

territories_counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

territories_counties %>%
  filter(state_abbv == "Oregon")

states %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), 
               fill = "grey", color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_text(data = get_urbn_labels(map = "states"), aes(x = long, lat, label = state_abbv), 
            size = 3)


household_data <- left_join(countydata, counties, by = "county_fips") 

View(countydata)
View(counties)

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

household_data %>%
  filter(state_name %in% c("Virginia", "Maryland", "District of Columbia")) %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::dollar) +
  labs(fill = "Median household income")

#for oregon
household_data %>%
  filter(state_name %in% c("Oregon", "Washington", "California")) %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::dollar) +
  labs(fill = "Median household income")

#let's get weird
household_data %>%
  filter(state_name %in% c("Washington", "Texas", "New York")) %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::dollar) +
  labs(fill = "Median household income")

#never stop not stopping
household_data %>%
  filter(state_name %in% c("Oregon", "Washington", "California")) %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::dollar) +
  labs(fill = "Median household income")+
  facet_wrap(. ~ state_name)

#let's throw caution to the wind
household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  facet_wrap(. ~state_name)
