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

graph_from_adjacency_matrix(em,mode=c("directed"))


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

# Closelness Centrality (CLC): measure defined for given vertex
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

# Puts weight of vertices into V(g)$weight
V(g)$weight<-rnorm(vcount(g)) 
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
plot(sg,edge.label=round(E(sg)$weight,3))

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

# Plot community 
plot(wc,sg,vertex.size=0.1,layout=layout.fruchterman.reingold)

# Alpha centrality 
acg <- alpha_centrality(g)
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







