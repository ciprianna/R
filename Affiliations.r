###Affiliation Network Test
library(igraph)
test<-read.csv("C:\\Users\\Ciprianna Dudding\\Desktop\\test.csv",header=TRUE,row.names=1,check.names=FALSE)
test
m=as.matrix(test)
g=graph.adjacency(m,mode="undirected",weighted=NULL)
g
Sam=c(1,1,0,0)
Tyler=c(1,0,1,0)
BMFK=c(0,1,0,1)
Jeremy=c(0,0,1,1)
bm=matrix(c(Sam,Tyler,BMFK,Jeremy),nrow=4,byrow=TRUE)
dimnames(bm)=list(c("Sam","Tyler","BMFK","Jeremy"),c("Pie","Fish","Candy","Steak"))
bm
bg=graph.incidence(bm) ##converts affiliation network into an igraph object
bg
###Should return:  IGRAPH UN-B 8 8 --
##########          +attr: type (v/x), name (v/c)
###This makes it an igraph object U (unweighted) N (named) with 8 nodes and 8 vertices
###Lists attributes, here type and name
V(bg)$type #list attribute type that differentiates which section of bipartite graph each node belongs to in order
V(bg)$name #list attribute name
####Graphing#####
shapes=c("circle","circle","circle","circle","square","square","square","square") ###This makes the first 4 nodes (people) circles and the groups squares
labeldistances=c(0,0,0,0,0.5,0.5,0.5,0.5) #Offsets labels for groups
plot(bg,vertex.shape=shapes,vertex.label.degree=-pi/2,vertex.label.dist=labeldistances,vertex.color=V(bg)$type)
###Create a one-mode projection
pr<-bipartite.projection(bg)
pr #this lists the two groups and labels them $proj1 and $proj2
get.adjacency(pr$proj1,sparse=FALSE,attr="weight") #Gives the adjacency matrix of the first projection
plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="black",vertex.label=V(pr$proj1)$name)
###This is the same as multiplying it by its transpose
aff=bm%*%t(bm)
diag(aff)=0
###Start Over###
library(igraph)
library(network)
library(sna)
library(statnet)
test<-read.csv("C:\\Users\\Ciprianna Dudding\\Desktop\\test.csv",header=TRUE,row.names=1,check.names=FALSE)
test
m=as.matrix(test)
g=graph.adjacency(m,mode="undirected",weighted=NULL)
g
V(g)$name #shows the name attribute from the igraph object,g
igraph.options(print.vertex.attributes=TRUE)
str(g)
###Now assign new attributes																												
V(g)$location<-as.character(location$location[match(V(g)$name,location$name)])
###Above line creates attribute location from the location vector in the attributes file and matches it to the name attribute in the matrix, g
V(g)$location #This lists the location attribute in node order
###Graphing
V(g)$color=V(g)$location #Assigns colors based on location attribute
V(g)$color=gsub("Waverly","Red",V(g)$color) #Waverly will be red
V(g)$color=gsub("Arlington","Green",V(g)$color) #Arlington will be green
V(g)$color=gsub("Omaha","Blue",V(g)$color) #Omaha will be blue
plot.igraph(g,vertex.label=V(g)$name,layout=layout.fruchterman.reingold)
V(g)$size<-degree(g)*5 #Sets the size of the nodes by degree
plot.igraph(g,vertex.label=V(g)$name,layout=layout.fruchterman.reingold)
plot(V(g)$pets) ##To plot an attribute  
which(V(g)$location=="Omaha")
##Lists node id's with the attribute location value of "Omaha"
unique(V(g)$location) #Lists the different values possible in location vector
from<-as.factor(V(g)$location) #Converts location attribute to a factor
summary(from) #Gives the number of times each unique value is used

