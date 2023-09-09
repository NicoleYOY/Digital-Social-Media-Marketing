
#install the required packages
#Package igraph provides tools for network analysis. The main goals of the igraph library are to provide a set of data types and functions for  pain-free implementation of graph algorithms and fast handling of large graphs, with millions of vertices and edges.

#The readr package provides a fast and friendly way to read data in a variety of types such as ??csv??.

  install.packages("igraph")
  install.packages("readr")

#load the packages

  library(igraph)
  library(readr)

#set working directory so that you can read file from that directory.

setwd("C:/Users/Nicole/Desktop/dataset") # set the working directory. This is the directiory where you data files are. You should set your own directory.

# The actors data frame is a list of all of the referenced (main) actors from the three previously mentioned movies. 
# The movies data frame contains connections between actors based on what movies they were in together.


actors = read_csv("Actors.csv")
movies = read_csv("Movies.csv")

#Create igraph object. if you do not have data frame for vertices, then you can use "vertices=NULL"
#The d variable takes the edges connecting the actor nodes that are held in the movies dataframe that was created. It requires a dataframe in first two columns of connections between vertice identifiers. 
#The vertices variable takes the actor nodes that are listed in the actors dataframe. It requires one column of node identifiers, which in this case are the actors?? names. 

actorNetwork = graph_from_data_frame(d=movies, vertices=actors, directed=F)

# plot the network.

plot(actorNetwork)

#remove the loops from the original network

network2=simplify(actorNetwork)

plot(network2)

# use different color to indicate the awards won by the actors.
# V()is for vertices and E() is for the edges.

V(network2)$color = ifelse(V(network2)$BestActorActress == "Winner", "gold",
                         ifelse(V(network2)$BestActorActress == "Nominated","grey",
                                "lightblue"))

plot(network2)


# add legend to the graph
legend("bottomright", c("Winner","Nominee", "Not Nominated"), pch=21,
  col="#777777", pt.bg=c("gold","grey","lightblue"), pt.cex=2, cex=0.5)



# node-level properties

moviedegree=degree(network2)

#moviecloseness=closeness(network2, weights=NA, normalized=T)

moviecloseness=closeness(network2,normalized=T)

moviebetween=betweenness(network2,normalized =T)

v1=sort(moviedegree,decreasing=TRUE)
v2=sort(moviecloseness,decreasing=TRUE)
v3=sort(moviebetween,decreasing=TRUE)


