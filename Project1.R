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

# Show v1
v1

# Show v2 
v2

# Get relations 
relations <- data.frame(from=v1,to=v2)  

# Get graph data frame 
g <- graph.data.frame(relations,directed=TRUE)


# Save original graph
oggraph <- g
# Show g
g

# Plot graph
plot(g)

# Centralize a graph according to the betweeness of its vertices
# Outputs centralizations and theoretical max 
centr_betw(g,directed=TRUE,nobigint=TRUE)

# Finding vertex attributes 
vertex_attr(g)

# Convert edges to adjacency matrix
# Converts g from a graph data frame to an adjacency matrix
adjmatrix <- as_adjacency_matrix(g)
adjmatrix 

# Puts weight of edges into E(g)$weight
E(g)$weight<-rnorm(ecount(g))

E(g)$weight

# Puts weight of vertices into V(g)$weight
V(g)$weight<-rnorm(vcount(g)) 

V(g)$weight 
g[1:5,1:9]

# Plot g 
plot(g)

# Check if graph is simple
is.simple(g)

# Copy the vertices with positive weight 
# into a subgraph and including the edge weight
# into plot (next slide)
sg <- induced.subgraph(g,which(V(g)$weight > 0.1))
sg

# Plot subgraph
plot(sg,edge.label=round((sg)$weight,3))

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



# 6. Determine the (a) central person(s) in the graph,
# (b) longest path, (c) largest clique 

# 6(a) Central Person(s) in the graph

# Simplified graph aka all data
centr_degree(simplifiedGraph)$centralization
# --The central degree is 4.67315 x 10^-6

# Just first 1000 rows 
centr_degree(simpleGraph)$centralization

# --The central degree is 0.006173605 
# Closeness Centrality 
centr_clo(simpleGraph,mode="all")$centralization

# 6(b) Longest path
# Check if graph is connected
is_connected(simpleGraph)

# It is not connected so unconnected is true
# The longest path algorithm is NP complete so
# diameter must refer to the longest path
diameter(simpleGraph,unconnected=TRUE)

# The longest path is 37

# 6(c) Largest clique 

# Experimenting with cliques function
cliques(simpleGraph,min=1,max=NULL)

# Largest cliques stored here in largestCliques variable
largestCliques <- max_cliques(simpleGraph,min=1,max=NULL,subset=NULL,file=NULL)

largestCliques
# THe largest clique is of size 3, there are multiple largest cliques

# 6(d) Ego
ego <- ego(simpleGraph)
ego

# 6(e) Betweenness centrality and power centrality 

# a. Is there more than one person with the most degrees?

# b. Are there multiple longest paths?

# c. Are there multiple cliques?

# d. Are there more than one person(s) with the highest ego?

# e. What is the difference in betweenness centrality vs. 
# power centrality for the cases you find? Consider 
# comparing the nodes that are members of each set. 
# Are there common nodes? 

# In each case what do you think the data tells you?

# 7. Find the 20 nodes with the greatest neighborhood out to a 
# distance 3 from the node. DO any of these neighborhoods overlap?

# 7a. Build a matrix of 20 nodes with their reachability to the 3rd level

# 7b. Determine which of the 20 nodes share common nodes, if any, and,
# for each common node, list the nodes that share that common node.



