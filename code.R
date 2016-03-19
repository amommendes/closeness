# Closeness Centrality

# setwd("PATH_TO_FILE_DIRECTORY")

# Reading Graph

g<-read.table("edges.dat",sep=" ")
g<- g+1 # Renaming nodes to indexing correctly below

head (g)
summary (g)

# Here I will tranform edges list in a Adjacencies matrix

gr<-table (g)
gr


# Number of Edges
nrow (g)
#Number of Vertices
ncol (gr)


# Here I will create adjacencies list that can be used in some algorithms,
# as Dijkstra Algorithm
 
adj<-function(g){
    nm<-sort(unique(g[,1]))
    l<- NULL
    for (i in 1:length(nm))
        l[[i]]<-c(g[which (g[,1]==i),2])
    return (l[1:length(nm)])
}

ad<-adj(g)

# Now we will implement (1) the Floyd-Warshall algorithm to define the shortest path
# between pairs and (2) assess the closeness centrality of graph vertices

short<- function (m){
    ## m = Adjacency Matrix
    # 1 - First we will define as Inf the distances between nodes without Edges
    for (i in 1:nrow(m)){ 
        for (j in 1:ncol (m))
            if (m[i,j] == 0 & i!=j )
                m[i,j]<- Inf}
    
    # 2 - Here we will calculate the shortest path between vertices, adding up the
    # distances between k intermediate nodes. 
    
    n<- ncol (m)
    for (k in 1:n)
        for (i in 1:n)
            for (j in 1:n)
                if (m[i,j] >  m[i,k] + m[k,j])
                    m[i,j]= m[i,k] + m[k,j]
    return (m)
 }


clos<- function (m, sort=T, verbose=F){
    ## m = Adjacency Matrix
    ## sort = should vertices be sorted by their closeness centrality?
    ## Verbose = should print result?
    m1<-short(m)
    c<- 1/(rowSums(m1))
    names(c)<-as.integer(names (c))-1
    
    if (verbose==T){ 
    print (paste('The node',names(c[c==max(c)]),
        'has the higest closeness centrality value in the rank'))}

    if (sort == T) {
        return (sort (c, decreasing = T))
    } else {return (c)}

}

init.time<-proc.time()
cc<-clos(gr)
proc.time() - init.time

# Here we will implement the BFS algorithm to define the shortest path (in 
# construction, meanwhile we implemented a test to assess if one nome is in the
# adjacencies of source node)

short2<- function (m, ad, s, d){
    if (!is.list(ad))
        stop("Did you provide a adjacencies list?!")
    dist<-NULL
    proc<-NULL
    parent<-NULL
    n<- ncol(m) # number of vertices
    for (i in 1:n){ 
        dist[i]<- Inf
        proc[i]<-FALSE
        parent[i]<- NULL}
    dist[s]<- 0
    
    # Test if node is adjacent to source
    
    if (m[s,d]==1){ 
        dist[d]<-1
        proc[c(d,s)]<-T
        return (data.frame(Nodes=which(dist!=Inf), Distances=dist[dist!=Inf],
                           Processed=proc[proc!=F]))
    }
    
}

short2(gr,ad,1,6)


# Using iGraph library
# this library uses C and is very fast...


g1<-graph.adjlist(ad)
graph.ad
ini.t<-proc.time()
a<-simplify(graph.adjacency(gr, mode = 'un'))
sort(closeness(a))
sort(closeness(g1))
proc.time() - ini.t

# Just for fun, we can visualize this graph
# (following Katherine Ognyanova tutorial - kateto.net/network-visualization)

## Creating a graph object with igrph package
library(igraph)
a<-simplify(graph.adjacency(gr, mode = 'un'))
plot (a)

# Some more packages
test.pack<- function (name=NULL)
    if (is.null(name)){
        warning('Please, provide a string with package name')
    } else { 
        if  (! name %in% rownames(installed.packages())){
            install.packages(paste(name))
        } else {
            
            library(paste(name), character.only = T)}
    }

test.pack("sna")
test.pack("ndtv")
test.pack('network')

## Plotting iGraph

plot(a,
     vertex.color=rgb(0, 0, .3, alpha=.8),
     vertex.frame.color='black',
     vertex.size=10,
     vertex.label.cex=.4,
     vertex.label.font=2,
     margin=c(0,0,0,0),
     main='Prospective customers social network',
     edge.curved=.3,
     layout= layout.kamada.kawai,
     mark.groups=45,mark.col="#C5E5E7",mark.border='black'
)
tkid <- tkplot(a) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(a, layout=l)


detach(package:igraph)

library(network)

c1<-clos(gr, sort = F) #closeness from clos function

net3 <- network(g, matrix.type="edgelist", loops=F, 
                directed = F,multiple=F, ignore.eval = F)

plot(net3, 
     vertex.cex= scale (c1),
     vertex.col="tomato"
     )

test.pack("networkD3")

g2<-data.frame (source=g$V1-1, target=g$V2-1)

n<-data.frame(name=as.factor(0:99),group=ifelse (c1 >= max(c1), "Influencer","Followers")
           , size=c1*2)

forceNetwork(Links = g2, Nodes = n, Source="source", Target="target",
             NodeID = "name",  Group = "group",linkColour = "#afafaf", 
             fontSize=10, zoom=T, legend=T, fontFamily = 'Calibri',
             colourScale = JS("d3.scale.category10()"),
             Nodesize='size', opacity = 0.8, charge=-500, bounded = F,
             width = 800, height = 600)    

