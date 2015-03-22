library("twitteR")
library(tm)
library(stringr)
library(igraph)
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "xxx"
consumerSecret <- "xxx"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=requestURL,accessURL=accessURL,authURL=authURL)
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem",package = "RCurl"))
registerTwitterOAuth(twitCred)
gaza = searchTwitter("#gaza", n = 1000,since="2014-07-01", cainfo="cacert.pem", retryOnRateLimit=1)
gaza
df <- do.call("rbind", lapply(gaza, as.data.frame))
dm_txt = sapply(gaza, function(x) x$getText())

Step 3: Identify retweets

grep("(RT|via)((?:\\b\\W*@\\w+)+)", dm_txt,ignore.case=TRUE, value=TRUE)
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)",dm_txt, ignore.case=TRUE)
dm_txt[rt_patterns]

Step 4: Who retweeted and who posted

who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))
for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = gaza[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}
# unlist
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

Step 5: Create graph from el

# two column matrix of edges
retweeter_poster = cbind(who_retweet, who_post)
# generate graph
#rt_graph = graph.edgelist(data)
rt_graph = graph.edgelist(retweeter_poster)
# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

Step 6: Plot

glay = layout.fruchterman.reingold(rt_graph)
# plot
par(bg="gray50", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,vertex.color="gray25",vertex.size=10,vertex.label=ver_labs,vertex.label.family="sans",vertex.shape="none",vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),vertex.label.cex=0.85,edge.arrow.size=0.8,edge.arrow.width=0.5,edge.width=3,edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
# add title
#title("\nTweets with 'bioinformatics':  Who retweets whom",cex.main=1, col.main="gray95")

V(rt_graph)$label.color = rgb(0,0,.2,.6)
V(rt_graph)$label.cex = .4
V(rt_graph)$size = 6
V(rt_graph)$frame.color = "black"
E(rt_graph)$color = rgb(.5,.5,0,.2)
E(rt_graph)$arrow.size = .1
E(rt_graph)$width = .3
plot(rt_graph, layout=layout.fruchterman.reingold)

# Degree 
V(rt_graph)$degree = degree(rt_graph)

# Betweenness centrality
V(rt_graph)$btwcnt = betweenness(rt_graph)
#plot(density(rt_graph))
V(rt_graph)$size = V(rt_graph)$degree/(max(V(rt_graph)$degree)/2)+.5
V(rt_graph)$label.cex = V(rt_graph)$degree/(max(V(rt_graph)$degree)/2)+.1
rt_graph = delete.vertices(rt_graph, V(rt_graph)[ degree(rt_graph)==0 ])

#cluster
clusters(rt_graph,mode="weak")
km<-as.vector(rt_graph$membership)
rt_graph$membership
km

#Color by cluster
colbar<-rainbow(196) #match the number to the number of clusters
#col<-colbar[km]

plot(rt_graph,edge.arrow.width=.6,edge.arrow.size=.1,vertex.color=colbar,vertex.size=V(rt_graph)$degree)
#rt2$layout = tkplot.getcoords(1)
