###SNA Test###
library(igraph)
library(network)
library(sna)
library(statnet)
library(ergm)
###Read delim
int<-read.delim("C://................",header=TRUE,sep="\t")
###Load data as a network/sna object###
int<-read.csv("C://Users//Ciprianna Dudding//Documents//R//Internet Traffic Hub and Spokes_Countries_196x196.csv",header=TRUE,blank.lines.skip=TRUE)
int<-int[1:196,2:197]
row.names(int)<-names(int)
int<-network(as.matrix(int),directed=TRUE)
int<-network(as.sociomatrix(int),directed=TRUE)
summary(int)
plot(int,displaylabels=TRUE,label.cex=0.5,boxed.labels=TRUE,vertex.cex=1)
#Or, remove isolates and make the vertices smaller; remove arrows
gplot(int,displaylabels=TRUE,label.cex=0.5,boxed.labels=TRUE,vertex.cex=1,displayisolates=FALSE,mode="springrepulse",usearrows=FALSE,object.scale=.008)
##Can also add: vertex.col=V(inti)$totaldeg ....to the plot to color nodes
	###Based on an attribute, in this case the total degree
ideg<-degree(int,cmode="indegree")
odeg<-degree(int,cmode="outdegree")
all(degree(int)==(ideg+odeg)) ##Should return TRUE if it works correctly
totaldeg<-degree(int,cmode="freeman")
plot(ideg,odeg,type="n",xlab="In Degree",ylab="Out Degree")##can leave
	### out type="n"
abline(0,1,lty=3)
plot(totaldeg)
hist((ideg+odeg/2),xlab="Total Degree", main="Degree Distribution", prob=TRUE)
###To turn into igraph###
int2<-as.matrix(int)
is.matrix(int2) ##Should return TRUE
inti<-graph.adjacency(int2,mode="directed",weighted=TRUE)
degdist<-degree.distribution(inti)
plot(degdist)
with(inti,text(degdist,labels=row.names(test),pos=4))##To add labels. 
	###Really messy, can't figure out how to change the label size, etc.
which(totaldeg>80) ##Lists the node id's that have total degree 
	###greater than 80 from the previous name totaldeg
V(inti)[188] #Lists the name of vertex 188
V(inti)$totaldeg<-totaldeg ##Assigns total degree as an attribute
bet<-betweenness(int)#Must calculate centrality scores from the network/SNA object
V(inti)$bet<-bet
V(inti)$bet
clo<-closeness(test)
V(inti)$clo<-clo
ev<-evcent(test)
V(inti)$ev<-ev
plot(V(inti)$totaldeg,xlab="Node ID",ylab="Degree",main="Degree Centrality")
cp<-cutpoints(int,mode="graph") #Lists the cutpoints in the graph from SNA object
gplot(int,vertex.col=2+cutpoints(int,mode="graph",return.indicator=TRUE))
gplot(int,vertex.col=2+cutpoints(int,mode="graph",return.indicator=TRUE),displayisolates=FALSE,usearrows=FALSE,displaylabels=TRUE,boxed.labels=TRUE,label.cex=0.5,label=cp)
gplot(int,vertex.col=2+cutpoints(int,mode="graph",return.indicator=TRUE),displayisolates=FALSE,usearrows=FALSE,displaylabels=TRUE if (cutpoints(int)=TRUE),boxed.labels=TRUE,label.cex=0.5)

##Plot the cutpoints
######STILL NEED TO FIGURE OUT: How to label only the nodes that are cutpoints
######Also, how to plot nodes above a threshold for a centrality score (betweenness) 
	###like we are plotting the cutpoints
table(component.dist(int)$csize) #Lists components and the size of the components
##So output above is:
#   1 176
#  20  1
# Meaning that there are 20xcomponents of size 1 and 1 component of size 176
gden(int) #Density of SNA object
grecip(int) #dyadic reciprocity
gtrans(int) #transitivity
cl<-component.largest(int,connected="weak")
cl
gplot(int[cl,cl],boxed.lab=FALSE,label.cex=0.5,label.col=4,displaylabels=TRUE,usearrows=FALSE,interactive=TRUE)
kcores(int) #lists k-cores by degree
##K-cores: a maximal connected subgraph of G in which all vertices have a degree at least k
##The degeneracy of G is the largest k for which G has a k-core
##A vertex u has a coreness C if it belongs to a c-core but not to any (C+1)-core
kc<-kcores(int,cmode="indegree") 
gplot(int,vertex.col=rainbow(max(kc)+1)[kc+1]) #plots by indegree
kct<-kcores(int,cmode="freeman")
gplot(int,vertex.col=rainbow(max(kct)+1)[kct+1]) #plots by all degree
gplot(int[kc>5,kc>5],vertex.col=rainbow(max(kc)+1)[kc[kc>5]+1],usearrows=FALSE,displaylabels=TRUE,label.cex=0.5,boxed.labels=TRUE)
##Above plot only plots the nodes with indegree >5 and colors them based on degree
gplot(int[kct>10,kct>10],vertex.col=rainbow(max(kct)+1)[kct[kct>10]+1],usearrows=FALSE,displaylabels=TRUE,label.cex=0.5,boxed.labels=TRUE)
##Same for the total degree above 10
components<-clusters(int)$membership
colors<-sample(rainbow(max(components)+1))
V(int)$color<-colors[components+1]
plot(int,layout=layout,vertex.label=NA,vertex.size=3)

###To make into a network object###
intn<-as.network.matrix(int2) ##Turns previous matrix object into network obj

#####Affiliations Networks#####
atts<-read.csv("C://Users//Ciprianna Dudding//Documents//R//Internet Traffic Hub and Spokes_Countries_Attributes.csv",header=TRUE,blank.lines.skip=TRUE)
atts<-atts[1:196,2:7]
row.names(atts)<-names(atts)
atts<-as.matrix(atts)
bm<-graph.incidence(atts, mode=c("all"))
bm
V(bm)$color[1:196]=rgb(1,0,0,.5)
V(bm)$color[197:202]=rgb(0,1,0,.5)
V(bm)$label=V(bm)$name
V(bm)$label.color=rgb(0,0,.2,.5)
V(bm)$label.cex=.4
V(bm)$size=6
V(bm)$frame.color=NA
pdf("Test Aff.pdf")
plot(bm,layout=layout.fruchterman.reingold)
dev.off()
bm<-as.numeric(atts)
gbm<-t(atts)%*%atts ###Gets the groups
gbm
gbmp<-atts%*%t(atts) ###Gets the members
gbmp
row.names(gbmp)<-names(gbmp)
gbmp<-network(as.matrix(gbmp),directed=TRUE)
gbmp<-network(as.sociomatrix(gbmp),directed=TRUE)
summary(gbmp)
gplot(gbmp,displaylabels=TRUE,label.cex=0.5,boxed.labels=TRUE,vertex.cex=1,displayisolates=FALSE,usearrows=FALSE,object.scale=.008)
gbmp2<-delete.vertices(gbmp,which(degree(gbmp)==0))
gbmp2<-as.matrix(gbmp)
is.matrix(gbmp2) #should return true
gi<-graph.adjacency(gbmp2,mode="undirected",weighted=TRUE)
gi
plot(gi)
E(gi)$weight<-count.multiple(gi)
V(gi)$label<-V(gi)$name
V(gi)$label.color<-rgb(0,0,.2,.8)
V(gi)$label.cex<-.6
V(gi)$size<-6
V(gi)$frame.color<-NA
V(gi)$color<-rgb(0,0,1,.5)
egam<-log(E(gi)$weight)+.3/max(log(E(gi)$weight)+.3)
E(gi)$color<-rgb(.5,.5,0,egam)
pdf("gi test.pdf")
plot(gi,main="layout.fruchterman.reingold",layout=layout.fruchterman.reingold)
plot(gi,main="layout.kamada.kawai",layout=layout.kamada.kawai)
dev.off()
gi<-graph.adjacency(gbmp2,mode="undirected",weighted=TRUE)
V(gi)$degree<-degree(gbmp) ##For some reason, have to reference the original network object
V(gi)$bet<-betweenness(gbmp)
E(gi)$arrow.size<-.3
V(gi)$label.cex<-V(gi)$degree/(max(V(gi)$degree)/2)+.3
plot(gi)



###First attribute###
oecd<-read.csv("C://Users//Ciprianna Dudding//Documents//R//Internet Traffic Hub and Spokes_Countries_Attributes.csv",header=TRUE,blank.lines.skip=TRUE)
oecd<-oecd[1:196,198:198] ###this assigns the oecd attribute from column 198
row.names(oecd)<-names(oecd)
oecd
V(g)$oecd<-oecd ##Assigns the oecd attribute to the igraph object g
V(g)$oecd
###This only works if they attributes are in the same order as those in the matrix
# Example, if you have an edgelist, will not be in order
###Second attribute###
geneva<-read.csv("C://Users//Ciprianna Dudding//Documents//R//Internet Traffic Hub and Spokes_Countries_Attributes.csv",header=TRUE,blank.lines.skip=TRUE)
geneva<-geneva[1:196,199:199]
row.names(geneva)<-names(geneva)
geneva
V(g)$geneva<-geneva
V(g)$geneva
summary(g) ##Should show the attributes as part of the igraph object now
###If not in order###
Atts<-read.csv("C://Users//Ciprianna Dudding//Documents//R//Internet Traffic Hub and Spokes_Countries_Attributes.csv",header=TRUE,blank.lines.skip=TRUE)
V(g)$oecd<-as.character(Atts$oecd[match(V(g)$names,Atts$Name)])
###Where Atts is the csv file with the attributes, oecd is the name of the column
##From that file, V(g)$names is how the nodes are identified, and Atts$Name
##Is what column that should match to from the Atts file


avector<-as.vector(aframe["a2"])
class(avector)

avector<-aframe[["a2"]]
class(avector)

avector<-aframe["a2"]
class(avector)

A data frame is a list.  When you subset a data frame using the name of a 
column and [ , what you're getting is a sublist (or a sub data frame).  If you 
want the actual atomic column, use [[ , or aframe [,2], which returns a vector
not a sublist

####TEST STUFF####
library(igraph)
testedge<-read.csv("C://Users//Ciprianna Dudding//Documents//R//sample_edgelist.csv",header=TRUE,blank.lines.skip=TRUE)
test<-graph.data.frame(testedge,directed=FALSE)
plot(test)

###Add attributes###
Atts<-read.csv("C://Users//Ciprianna Dudding//Documents//R//sample_actors.csv",header=TRUE,blank.lines.skip=TRUE)
V(test)$name<-as.character(Atts$Actors[match(V(test)$name,Atts$ID)])
V(test)$name
V(test)$label<-V(test)$name
plot(test)
V(test)$hair<-as.character(Atts$Hair[match(V(test)$name,Atts$Actors)])
V(test)$hair
V(test)$doctor<-as.character(Atts$Doctor[match(V(test)$name,Atts$Actors)])
V(test)$doctor

###Centrality###
V(test)$totdeg<-degree(test)
V(test)$totdeg
summary(V(test)$totdeg)
V(test)$bet<-betweenness(test)
V(test)$betN<-betweenness(test,normalized=TRUE)
V(test)$bet
summary(V(test)$bet)
V(test)$betN
V(test)$clo<-closeness(test)
V(test)$clo<-closeness(test,normalized=TRUE)
summary(V(test)$clo)

###Visualizations###
V(test)$color<-ifelse(V(test)$doctor=="1","lightblue","green")
V(test)$label.cex<-V(test)$bet/(max(V(test)$bet)/2)+.5
E(test)$color<-"darkgray"
plot(test)
legend("topleft",title="Position",fill=c("lightblue","green"),c("Doctor","Resident"))
##To color by centrality
##V(test)$color[which V(test)$bet>.15]=rgb(.2,0,.5)

###Community Structures###
wc<-walktrap.community(test,steps=2)
wc #Lists the names and the number of the community they belong to
wc<-as.vector(wc)
V(test)$wc<-wc
V(test)$wc
Young<-induced.subgraph(test,communities(wc)[[3]])
Middle<-induced.subgraph(test,communities(wc)[[1]])
Older<-induced.subgraph(test,communities(wc)[[2]])
plot(Young)
plot(Middle)
plot(Older)
##Overlay communities##
plot(test)
plot(Young,add=TRUE) #Add=TRUE plots this graph on top of the previous plot
##Color communities##
colbar<-c(rgb(.5,0,0,.5),rgb(0,.5,.2,.5),rgb(.2,.2,.6,.5)) #Gives specific colors
#to groups with transparency
#colbar<-rainbow(3) #To auto assign rainbow colors to the three groups
col<-colbar[wc$membership]
plot(test,vertex.color=col,main="Social Communities")
#Used the walktrap community: finds communities via short, random walks based
#  on the number of steps; could also use:
#edge.betweenness: Densely connected internally, sparsely connected to other 
#  modules; high betweenness separates communities
#fastgreedy: Uses a directly optimizing modularity score
#label.propagation: Labels the unique vertices and updates labels by majority
#  vote in the vertex neighborhood
#leading.eigenvector: Calculates the leading nonnegative eigenvector of the 
#  modularity matrix of the graph
#multilevel: Multi-level modularity optimization algorithm; hierarchical approach
#optimal: Maximizes the modularity measure over all possible partitions
#spinglass: Uses an energy function for connections within compared to outside
#  of communities

###Test community significance (only in directed graphs)###
cst<-function(test,wc){
if(is.directed(test))stop("This method requires an undirected graph")
in.degrees<-degree(Young)
out.degrees<-degree(test,wc[[3]])-in.degrees
wilcox.test(in.degrees,out.degrees)
}
##This is supposed to compare in degree and out degree; if a community has more
#in than out, it is "important" to the network.  Null hypothesis is that the
#in and out degree will be the same.

###Egonets###
Cristina<-graph.neighborhood(test,1,nodes="Cristina")
Cristina
George<-graph.neighborhood(test,1,nodes="George")
George
plot(Cristina[[1]])
plot(George[[1]])
#Could use some way to overlay or discover who's in both neighborhoods
#and how much overlap there is (as a percentage maybe?)

