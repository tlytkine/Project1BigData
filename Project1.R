## Download and install the package
install.packages("igraph")

# Install SNA package
install.packages("sna")

## Load packages
library(igraph)
library(sna)

# Get edges from text file, creates data frame 
edges1 <- read.table("roadNet-CA.txt")

# Create iGraph from data frame 
edgesGraph <- graph_from_data_frame(edges1,directed=TRUE,vertices=NULL)

# Simplify graph
simplifiedGraph <- simplify(edgesGraph,remove.multiple=TRUE,remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))

# is graph simple?
is_simple(simplifiedGraph)

# Convert edges to matrix
em <-as.matrix(edges1)


# First 1000 rows of the data as a matrix
rows <- em[0:1000,]
allRows <- em[0:5533214,]

# Show the rows 
rows

# Degree of the first 1000 rows 
degree(rows)

# Density of the first 1000 rows
gden(rows)

# Connectedness of first 1000 rows (1 means graph is connected)
connectedness(rows)

# geodisc: shortest path between any two nodes in the networks
geodist(rows)

# Egocentric network of a vertex v is a subgraph consisting of v 
# and its immediate neighbors. The ego neighborhood for v1 is shown
# below for 10 rows 
ego <- ego.extract(rows)
ego[1:3]

# Closeness Centrality (CLC): measure defined for given vertex
# Higher the closeness, the closer the node is to the other nodes
# in the graph
# The ratio of the total number of nodes in the graph minus one
# to the sum of the geodesics from v to every vertex in the graph
closeness(ego)

# Get vertices from first column
v1 <- rows[0:1000,1]


# Get vertices from second column 
v2 <- rows[0:1000,2]
allV2 <- allRows[0:5533214,2]

# Show v1
v1

# Show v2 
v2

# Get relations 
relations <- data.frame(from=v1,to=v2)  
allRelations <- data.frame(from=allV1,to=allV2)

# Get graph data frame 
g <- graph.data.frame(relations,directed=TRUE)

allG <- graph.data.frame(allRelations,directed=TRUE)

# Simplify graph with all rows 
simpAll <- allG
# simpAll <- simplify(allG,remove.multiple=TRUE,remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))
# g <- simplify(g,remove.multiple=TRUE,remove.loops=TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))


# Save original graph
oggraph <- g
# Show g
g
# Plot graph
plot(g, vertex.label = NA)



# Centralize a graph according to the betweeness of its vertices
# Outputs centralizations and theoretical max 
centr_betw(g,directed=TRUE,nobigint=TRUE)
# centr_betw(simpAll,directed=TRUE,nobigint=TRUE)

# Finding vertex attributes 
vertex_attr(g)

# Convert edges to adjacency matrix
# Converts g from a graph data frame to an adjacency matrix
adjmatrix <- as_adjacency_matrix(g)
adjmatrix 

# Puts weight of edges into E(g)$weight
E(g)$weight<-rnorm(ecount(g))

E(g)$weight

E(simpAll)$weight <- rnorm(ecount(simpAll))
E(simpAll)$weight

# Puts weight of vertices into V(g)$weight
V(g)$weight<-rnorm(vcount(g)) 

V(g)$weight 
g[1:5,1:9]

V(simpAll)$weight <- rnorm(vcount(simpAll))
V(simpAll)$weight 

# Plot g 
plot(g)

# Check if graph is simple
is.simple(g)
is.simple(simpAll)

# Copy the edges/vertices with positive weight 
# into a subgraph and including the edge weight
# into plot (next slide)
sg <- induced.subgraph(g,which(V(g)$weight > 0.1, E(g)$weight > 0.1))
sg



vcount(simpAll)
# Original v count = 1965206
ecount(simpAll)
# Original e count = 5533214

sg1 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.1, E(simpAll)$weight > 0.1))
sg1
vcount(sg1)

# sg1 v count = 904402
ecount(sg1)
# sg1 e count = 1171628

sg2 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.2, E(simpAll)$weight > 0.2))
sg2
vcount(sg2)
# sg2 v count = 8269246
ecount(sg2)
# sg2 e count = 979338

sg3 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.3, E(simpAll)$weight > 0.3))
sg3
vcount(sg3)
# sg3 v count = 750912
ecount(sg3)
# sg3 e count = 806630

sg4 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.4, E(simpAll)$weight > 0.4))
sg4
vcount(sg4)
# sg4 v count = 677038
ecount(sg4)
# sg4 e count = 655274

sg5 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.5, E(simpAll)$weight > 0.5))
sg5
vcount(sg5)
# sg5 v count = 606737
ecount(sg5)
# sg5 e count = 526676

sg6 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.6, E(simpAll)$weight > 0.6))
sg6
vcount(sg6)
# sg6 v count = 539353
ecount(sg6)
# sg6 e count = 416078

sg7 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.7, E(simpAll)$weight > 0.7))
sg7
vcount(sg7)
# sg7 v count = 476138
ecount(sg7)
# sg6 e count = 324492

sg8 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.8, E(simpAll)$weight > 0.8))
sg8
vcount(sg8)
# sg8 v count = 416808
ecount(sg8)
# sg8 e count = 248380

sg9 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.9, E(simpAll)$weight > 0.9))
sg9
vcount(sg9)
# sg9 v count = 362127
ecount(sg9)
# sg9 e count = 187172

sg9
# AS STAR WORKS!!
# plot(sg9, layout=layout_as_star, vertex.color="green",vertex.label=NA)
# As circle works too
# plot(sg9, layout=layout_in_circle, vertex.color="green",vertex.label=NA)
# As grid works 
# plot(sg9, layout=layout_on_grid, vertex.color="green",vertex.label=NA)

# plot(sg9, layout=layout_components, vertex.color="green",vertex.label=NA)

# plot(sg9, layout=layout_on_sphere, vertex.color="green",vertex.label=NA)

# plot(sg9, layout=layout_randomly, vertex.color="green",vertex.label=NA)

# plot(sg9, layout=layout_with_lgl, vertex.color="green",vertex.label=NA)

# plot(simpleGraph, layout=layout_with_graphopt, vertex.color="green",vertex.label=NA)




# Check if subgraph is simple 
# A graph without loops and multiple edges
# between vertices is called simple
is.simple(sg)

# Simplify original graph
simpog <- simplify(oggraph)

# Check if simplified graph is simple
is.simple(simpog)

# Plot simplified graph
plot(simpog)

# Show simpog
simpog

# Page Rank of original graph
page_rank(oggraph)

# Page Rank of original graph with weights
page_rank(g)

# Page Rank of simplified graph
page_rank(sg)

# Suppose we need to find densely connected
# subgraphs, also called communities in a graph
# via random walks. The idea is that short random
# walks tend to stay in the same community.
wc <- walktrap.community(sg)

wc

# Plot community 
plot(wc,sg,vertex.size=0.1,layout=layout.fruchterman.reingold)

# Alpha centrality 
acg <- alpha_centrality(g,nodes=V(g),alpha=1,loops=FALSE)
acg

# Cliques 
# Find the # of disjoint paths between two vertices 
# Between vertex 1 and vertex 2 
edge.disjoint.paths(g,"1","2")

# More about R 
# Special constants include:
# NA for missing or undefined data
# NULL for empty object (e.g. null/empty lists)
# Inf and -Inf for positive and negative infinity 

# NaN for results that cannot be reasonably defined
# check if missing
is.na(5+NA)

# NULL - an empty object e.g. a null / empty list
is.null(NULL)

# is.finite
is.finite(5/0)

# is.nan (not a number)
is.nan(0/0)


# em is a matrix 
# em is everything as a matrix
em.df <- as.data.frame(em)
em.df

# k means clustering 
kc <- kmeans(em.df,3,nstart=5)
kc

#The edge connectivty of the simplifiedGraph
edge_connectivity(simplifiedGraph,1,5)

# Edge_disjoint path 

edge_disjoint_paths(simplifiedGraph,1,5)

simplifiedGraphTest <- sample_gnm(n=10,m=25)
simplifiedGraphTest

plot(simplifiedGraphTest)

# Taking random sample from subgraph of data set and plotting 
smpl1 <- sample(1:5,2)
V(sg)$randomSample <- "1"
V(sg)[smpl1]$randomSample <- "2"

V(sg)$color = "#F5E31C" 
V(sg)[smpl1]$color="#1CA38B" 

set.seed(123)
plot(sg, vertex.label.color = "Black",vertex.size = 1,layout = layout.fruchterman.reingold(sg))


# Experimenting with the edge_connectivity function
simplifiedGraph

numEdges <- ecount(simplifiedGraph)

print(numEdges)

start <- 1
end <- numEdges

var <- edge_connectivity(simplifiedGraph, 1, 2)
print(var)
var1 <- edge_connectivity(simplifiedGraph, 1, 1000)
print(var1)


# Vertices of simplified graph
simpleGraph <- oggraph


# simpleGraph just contains 1000 rows 
V(simpleGraph)
E(simpleGraph)

# simplifiedGraph contains all data with 
# multiple loops and edges removed

# Vertices of simplified Graph
V(simplifiedGraph)
# Edges of simplified graph
E(simplifiedGraph)
# plot simplified graph
plot(simpleGraph)
# Now simplify it 
simplifiedGraph <- simplify(simpleGraph)
# plot it again
plot(simplifiedGraph)

# Edge Connectivity of graph
adhesion(simplifiedGraph)

# Convert to directed Graph
simplifiedGraph <- as.directed(simplifiedGraph)

simplifiedGraph
# Plot 
plot(simplifiedGraph)

# Vertex count in simplified Graph
vcount(simplifiedGraph)
# View vertices 
V(simplifiedGraph)
# View edges
E(simplifiedGraph)

# Show all simple paths from 1 to 5 
all_simple_paths(simplifiedGraph,1,5)

# Make sure there are no multiple edges in the graph
which_multiple(simplifiedGraph,E(simplifiedGraph))

# Find triangles in the graph
triangles <- count_triangles(simpleGraph)
triangles

# Distances 
distances(simpleGraph)

# Degree Distribution
degree_distribution(simpleGraph)

# Shortest Path from 1 to 5 ex: 0 1 6 
shortest_paths(simpleGraph,1,5)

# Size of the graph / number of edges 
gsize(simpleGraph)
# Outputs 1000

# Eccentricity of a vertex is its shortest path 
# distnace from the farthest other node in the graph
eccentricity(simpleGraph)

# Order (number of vertices of a graph)
gorder(simpleGraph)

# Average nearest neighbor degree
knn(simpleGraph)

# Head of the edges in a graph 
head_of(simpleGraph,E(simpleGraph))

igraph_demo()
igraph_demo("centrality")

# Make label the same as names 
V(simpleGraph)$label <- V(simpleGraph)$name
V(simpleGraph)$label
V(simpleGraph)

### Take a look at it
plotG <- function(simpleGraph) {
  plot(simpleGraph, asp=FALSE, vertex.label.color="blue", vertex.label.cex=0.1,
       vertex.label.font=0.1, vertex.size=0.5, vertex.color="white",
       vertex.frame.color="white", edge.color="black")
}
plotG(g)


# Tails of the edges in the graph
tail_of(simpleGraph,E(simpleGraph))

# Experimenting with different layouts
plot(simpleGraph, layout=layout_with_kk, vertex.color="green", vertex.label=NA)

plot(simpleGraph, layout=layout_as_star, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_as_tree, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_in_circle, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_nicely, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_as_star, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_with_dh, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_with_gem, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_with_graphopt, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_on_grid, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_with_mds, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_components, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_on_sphere, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_randomly, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_with_lgl, vertex.color="green",vertex.label=NA)

plot(simpleGraph, layout=layout_with_graphopt, vertex.color="green",vertex.label=NA)

ecount(simplifiedGraph)
vcount(simplifiedGraph)

oldSimplifiedGraph <- simplifiedGraph
simplifiedGraph <- sg9



# 6. Determine the (a) central person(s) in the graph,
# (b) longest path, (c) largest clique 

# 6(a) Central Person(s) in the graph

# Simplified graph aka all data
centr_degree(simplifiedGraph)$centralization
# --The central degree is 1.514154 * 10 ^ -5 

# Closeness Centrality 
centr_clo(simplifiedGraph,mode="all")$centralization
# 1.860223 * 10 ^-10

# 6(b) Longest path
# Check if graph is connected
is_connected(simplifiedGraph)


# It is not connected so unconnected is true
# The longest path algorithm is NP complete so
# diameter must refer to the longest path
diameter(simplifiedGraph,unconnected=TRUE)

# The longest path is 37

# 6(c) Largest clique 

# Experimenting with cliques function
cliques(simplifiedGraph,min=1,max=NULL)

# Largest cliques stored here in largestCliques variable
largestCliques <- max_cliques(simpifiedGraph,min=1,max=NULL,subset=NULL,file=NULL)

largestCliques
# THe largest clique is of size 3, there are multiple largest cliques

# 6(d) Ego
ego <- ego(simplifiedGraph)
ego

# 6(e) Betweenness centrality and power centrality 

# a. Is there more than one person with the most degrees?
Yes
# b. Are there multiple longest paths?
Yes
# c. Are there multiple cliques?
Yes
# d. Are there more than one person(s) with the highest ego?
Yes
# e. What is the difference in betweenness centrality vs. 
# power centrality for the cases you find? Consider 
# comparing the nodes that are members of each set. 
# Are there common nodes? 

# In each case what do you think the data tells you?

# 7. Find the 20 nodes with the greatest neighborhood out to a 
# distance 3 from the node. DO any of these neighborhoods overlap?
# https://igraph.org/r/doc/communities.html
wc1 <- cluster_walktrap(simpleGraph)
modularity(wc1)
membership(wc1)
sizes(wc1)

plot(wc1,sg,vertex.size=0.1,layout=layout.fruchterman.reingold)

# 7a. Build a matrix of 20 nodes with their reachability to the 3rd level

# 7b. Determine which of the 20 nodes share common nodes, if any, and,
# for each common node, list the nodes that share that common node.



