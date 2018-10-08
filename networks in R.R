#THINGS TO KNOW
#NETWORK OPERATORS HAVE SOME VERY SPECIAL PIPES
#%v% - is common
#THIS IS NOT THE TIDYVERSE!!!!!!!!!
#THINGS ACTUALLY GET CHANGED HERE
#less hyperbolically, there are over twenty network specific pipes that predate magrittr
#these pipes are powerful and the documentation is less than awesome
#network and sna are old packages from before this new wave of popularity for R
library(rvest)
library(dpyr)
library(ggplot2)
library(network)
library(sna)
library(ggnetwork)

#DO NOT LOAD library(igraph) - it disrupts library(network)

#Go to the code from last week: update the football data and add it here
n<-network(clean_football, directed = TRUE, matrix.type = "edgelist")


# with nodes colored according to betweenness centrality
set.vertex.attribute(n, "conference", conf_data$CurrentConference)
set.vertex.attribute(n, "state", conf_data$State.1.)
n %v% "betweenness" <- betweenness(n)
n %v% "closeness" <- closeness(n)
n %v% "bonpow" <- bonpow(n, rescale = TRUE)
n %v% "hierarchy" <- hierarchy(n)
n %v% "kcores" <- kcores(n)
n %v% "evcent" <- evcent(n)
list.vertex.attributes(n)

#edge attributes can also be set
e <- network.edgecount(n)
set.edge.attribute(n, "type", conf_data$Nickname, e)
set.edge.attribute(n, "score", foot$Pts, e)

#old operators
network.layout.circle(n)

#layout methods
H<-ggnetwork(n, layout = "circle")

#example with controls
J<-ggnetwork(n, layout = "fruchtermanreingold")
JJ<-ggnetwork(n, layout = "fruchtermanreingold", layout.par = list(cool.exp = 50, repulse.rad=500, cell.jitter = 15, niter = 5000))

#the documentation on implementing controls is just awful
#?gplot.layout

#see http://melissaclarkson.com/resources/R_guides/documents/gplot_layout_Ver1.pdf
#for more on the layouts

#a whole mess of layouts assigned to ggnetwork models
K<-ggnetwork(n, layout = "kamadakawai")
L<-ggnetwork(n, layout = "circrand")
M<-ggnetwork(n, layout = "geodist")
N<-ggnetwork(n, layout = "hall")
O<-ggnetwork(n, layout = "mds")
P<-ggnetwork(n, layout = "target")
Q<-ggnetwork(n, layout = "princoord")
R<-ggnetwork(n, layout = "random")
S<-ggnetwork(n, layout = "rmds")
U<-ggnetwork(n, layout = "segeo")
V<-ggnetwork(n, layout = "seham")
W<-ggnetwork(n, layout = "spring")
B<-ggnetwork(n, layout = "springrepulse")
C<-ggnetwork(n, layout = "eigen")


# just nodes
ggplot(JJ, aes(x, y)) +
  geom_nodes(size = 3, shape = 21, color = "steelblue") +
  theme_blank()

# with edges
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "steelblue") +
  geom_nodes(size = 3, shape = 21, color = "steelblue", fill = "white") +
  theme_blank()

ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(size = degree(n), aes(color = bonpow), size = hierarchy) +
  scale_color_gradient(low = "gold", high = "tomato") +
  geom_nodetext_repel(aes(label = vertex.names, size = closeness))+
  theme_blank() +
  theme(legend.position = "bottom")


ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = score), color = "grey50") +
  geom_nodes(size = degree(n), aes(color = kcores), size = closeness) +
  geom_nodetext_repel(aes(label = vertex.names, size = closeness))+
  theme_blank() +
  theme(legend.position = "bottom")

#the thing that will likely frustrate you the most is the difficulty in doing modularity analysis in SNA/gg

??geom_edges


#here is another world
#network != graph
#if you want to do this, look at Dr. Katya Ognyanova's work

#open a new script file, Let's run Dr. Ognyanova's code: 
"http://kateto.net/netscix2016"


#slug: running this on our own
library(igraph)

nn<-graph_from_data_frame(clean_football, directed = TRUE)
nn
class(nn)
V(nn)
nn2<-cluster_walktrap(nn)
modularity(nn2)
nn3<-membership(nn2)

plot(nn, vertex.shape="none", vertex.label=V(nn)$vertex.names, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")
