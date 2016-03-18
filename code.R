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


clos<- function (m, sort=T){
    ## m = Adjacency Matrix
    ## sort = should vertices be sorted by their closeness centrality?
    
    m1<-short(m)
    c<- 1/(rowSums(m1))
    
    if (sort == T) {
        return (sort (c))
    } else {return (c)}
    
}

init.time<-proc.time()
cc<-clos(gr)
init.time -proc.time()


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

library(igraph)

ini.t<-proc.time()
a<-simplify(graph.adjacency(gr, mode = 'un'))
sort(closeness(a))
proc.time() - ini.t
