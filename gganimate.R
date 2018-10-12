#GGanimate tutorial 

library(ggplot2)
library(gganimate)
library(dplyr)


#its not formally avaialble so we are going old school with dev tools
devtools::install_github('thomasp85/gganimate')#devtools::install_github('thomasp85/ggforce')
#devtools::install_github("dgrtwo/gganimate")
devtools::install_github('thomasp85/ggraph')

#the render times will be far slower than you are used to with your video software

#this is his primary example - isn't it nice that he writes code like we like
ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#we will use this on monday as well
library(gapminder)

#here is their example
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

#method to export the gif
anim_save("gap.gif")



#so this is a discrete/contniuous 
#let's get one of our own datasets here
#RUN THE COMPLETE FOOTBALL PROGRAM
#THEN THE BIGFOOT CHUNK
conf_data2<-rename(conf_data, Winner=vertex.names)
BigFoot<-inner_join(foot2, conf_data2, by = "Winner")

BigFoot<-BigFoot%>%
  filter(Rk<407)


#NOW we run this like gapminder
ggplot(BigFoot, aes(factor(CurrentConference), Pts)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    Pts.1,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#this is pretty sick, like an 80s sci-fi thing.

#RUN networks in R LINES 9-96 ONLY!!!!!!!
#THIS WILL TAKE ABOUT TWO MINUTES

ggplot(J, aes(x, y)) +
  geom_nodes(size = 3, shape = 8, color = "pink") +
  theme_blank()+
  transition_states(
    conference,
    transition_length = 2,
    state_length = 1,
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#moar detail
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "orange") +
  geom_nodes(size = degree(n), aes(color = bonpow)) +
  scale_color_gradient(low = "gold", high = "tomato") +
  geom_nodetext_repel(aes(label = vertex.names, size = closeness))+
  theme_blank() +
  theme(legend.position = "bottom")+
  transition_states(
    conference,
    transition_length = 2,
    state_length = 1,
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#similar
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = score), color = "grey50") +
  geom_nodes(size = log(degree(n)), aes(shape = conference, color = kcores)) +
  geom_nodetext_repel(aes(label = vertex.names, size = closeness))+
  theme_blank() +
  theme(legend.position = "bottom")+
  transition_states(
    conference,
    transition_length = 2,
    state_length = 1,
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#and you can always save the RHS (file) from the last function. 
anim_save("feet.gif")

#state
p <- ggplot(iris, aes(Sepal.Width, Petal.Width)) +
  geom_point() +
  #this is labeling the state of the animation
  labs(title = "{closest_state}") +
  transition_states(Species, transition_length = 3, state_length = 1)+
  

#you can also call - next_state or previous_state or transitioning

#really elegant faceting example
animate(p)
last_animation()




#let's grab a transition reveal
#this is a really solid graphic
p <- ggplot(airquality, aes(Day, Temp, group = Month)) +
  geom_line() +
  transition_reveal(Month, Day)

animate(p)

#let's rewrite this
library(ggvis)
head(cocaine)
ggplot(cocaine, aes(month, potency, group = state, colour = state))+
  geom_line()+
  transition_reveal(price, price)

ggplot(cocaine, aes(month, potency, group = state, colour = state))+
  geom_jitter()+
  #lets play with this
  transition_reveal(month, price)

anim_save("cocaine.gif")

#let's make this more easily rendered
cocaine%>%
  filter(state == c("TX", "CA", "FL"))%>%
  ggplot(aes(month, potency, group = state, colour = state))+
  geom_line()+
  transition_reveal(price, price)

#let's try a different geom
cocaine%>%
  filter(state == c("TX", "CA", "FL"))%>%
  ggplot(aes(month, potency, group = state, colour = state))+
  geom_jitter()+
  transition_reveal(month, price)

#good now lets add a month label
p <- ggplot(cocaine, aes(potency, price)) +
  geom_point() +
  #this is labeling the state of the animation
  labs(title = "{closest_state}") +
  transition_states(month, transition_length = 3, state_length = 1)
animate(p)  
#oh good that worked

#and a manual transition
ggplot(cocaine)+
  geom_boxplot(aes(factor(price), potency))+
  transition_manual(state)


#Let't jazz this up - whoa - the color render is really hard for the computer
p <- ggplot(cocaine, aes(potency, price, color=state)) +
  geom_point() +
  #this is labeling the state of the animation
  labs(title = "{closest_state}") +
  transition_states(month, transition_length = 3, state_length = 1)
animate(p)  

#to work through all possible effects, we need a simple plot
C<-filter(cocaine, state == c("TX", "CA", "FL"))
#IGNORE WARNING

#GGANIMATE DOES NOT HANDLE PIPES WELL!
  ggplot(C, aes(month, potency, group = state, colour = state))+
  geom_jitter()+
  transition_states(month, transition_length = 3, state_length = 1)
  
  #NICE PLOT! Let's store this.
D<-ggplot(C, aes(month, potency, group = state, colour = state))+
    geom_jitter()+
    transition_states(month, transition_length = 3, state_length = 1)

#var one - total frames, frames per second, duration, detail, renderer, device, ref_frame)
animate(D, 240, 24)  

#enter_exit effects
D+enter_fade()
D+enter_fade()+enter_appear(early = TRUE)+exit_shrink()
D+enter_grow()+exit_shrink()

#Let's play with a topological build

E<-ggplot(C, aes(month, potency, group = state, colour = state))+
  geom_jitter()


E+transition_layers(layer_length = 2, transition_length = 2, keep_layers = TRUE, from_blank = TRUE, layer_names = NULL)

#developed last month
ggplot(diamonds, aes(cut, carat)) +
  geom_point(col = "red") +
  geom_boxplot() + 
  transition_layers(layer_length = 1, transition_length = 1)

ggplot(BigFoot, aes(Wk, Pts, color=CurrentConference))+
  geom_point()+
  geom_rug()+
  transition_layers(layer_length = 2, transition_length = 2)


#argument passing is in the documentation with no examples
#it appears that multiple renderers are possible, including FFMPEG, and 
animate(D, renderer = gifski_renderer)
library(av)
#this code should work but isn't quite there yet
animate(D, av_renderer(file = tempfile(fileext = ".mp4"), vfilter = "null", codec = NULL, audio = "/Users/ACE/Downloads/this_party.wav"))
#or you can pull code like this to add a sound file. Well, honestly you should do this in your DNLE.

#there is no sample code for the functions and they appear to call third-libraries
#reveal the underlying structure of the code by running it without ()
#notice a lot of for-loops and knitr
animate
av_renderer

#the gif size can also be changed
#options(gganimate.dev_args = list(width = 800, height = 600))




