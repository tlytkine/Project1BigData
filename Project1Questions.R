## Load packages
library(igraph)
library(sna)

# Get edges from text file, creates data frame 
edges1 <- read.table("roadNet-CA.txt")

# Convert edges to matrix
em <-as.matrix(edges1)

# All rows of data 
rows <- em[0:1000,]
allRows <- em[0:5533214,]

# Density of rows: 0.7083459
gden(allRows)

# Get vertices from first column 
allV1 <- allRows[0:5533214,1]
v1 <- rows[0:1000,1]

# Get vertices from second column
allV2 <- allRows[0:5533214,2]
v2 <- rows[0:1000,2]

# Get Relations 
allRelations <- data.frame(from=allV1,to=allV2)
relations <- data.frame(from=v1,to=v2)  

# Get graph data frame 
allG <- graph.data.frame(allRelations,directed=TRUE)
g <- graph.data.frame(relations,directed=TRUE)
simpAll <- allG 

# Put weight of edges into E(simpAll)$weight 
E(simpAll)$weight <- rnorm(ecount(simpAll))
E(simpAll)$weight
E(g)$weight<-rnorm(ecount(g))
E(g)$weight

# Put weight of vertices into V(simpAll)$weight 
V(simpAll)$weight<-rnorm(vcount(simpAll)) 
V(simpAll)$weight 
V(g)$weight<-rnorm(vcount(g)) 
V(g)$weight 


# Check if graph is simple 
is.simple(simpAll)

# Vertex count of simpAll
vcount(simpAll)

# Edge count of simpAll
ecount(simpAll)

# Simplify graph 

sg9 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.9, E(simpAll)$weight > 0.9))
sg9
sg1 <- induced.subgraph(g,which(V(g)$weight > 0.1, E(g)$weight > 0.1))
sg1
vcount(sg9)
# sg9 v count = 362127
ecount(sg9)
# sg9 e count = 187172
vcount(sg1)
ecount(sg1)


simplifiedGraph <- sg9

# 4. 10 Functions in the slides (at least)
# 1.
degree(rows)

# 2.
closeness(ego)

# 3. 
# Save original graph
oggraph <- g
# Show g
g
# Plot graph
plot(g, vertex.label = NA)

# 4.
centr_betw(g,directed=TRUE,nobigint=TRUE)

# 5. 
vertex_attr(g)

# 6.
adjmatrix <- as_adjacency_matrix(g)
adjmatrix 

# 7.
is.simple(g)

# 8.
# Simplify original graph
simpog <- simplify(oggraph)

# 9. 
# Page Rank of original graph with weights
page_rank(g)

# 10.
# Alpha centrality 
acg <- alpha_centrality(g,nodes=V(g),alpha=1,loops=FALSE)
acg

# 5 . 15 functions not in slides (at least)
# Find triangles in the graph
#1
triangles <- count_triangles(simpleGraph)
triangles

#2
gsize(simpleGraph)

#3
# Eccentricity of a vertex is its shortest path 
# distnace from the farthest other node in the graph
eccentricity(simpleGraph)

#4
power_centrality(sg1, exponent=0.9)

#5
# Largest cliques stored here in largestCliques variable
largestCliques <- max_cliques(simpleGraph,min=1,max=NULL,subset=NULL,file=NULL)

#6
# Experimenting with cliques function
cliques(simpleGraph,min=1,max=NULL)

#7
neighbors(simpleGraph)

#8
print.igraph(simpleGraph)

#9
distances(simpleGraph)

#10
diameter(simpleGraph)

#11
degree_distribution(simpleGraph)

#12
# Show all simple paths from 1 to 5 
all_simple_paths(simplifiedGraph,1,5)

#13
edge_connectivity(simpleGraph, source = NULL, target = NULL, checks = TRUE)

#14
# Head of the edges in a graph 
head_of(simpleGraph,E(simpleGraph)

#15
# Tails of the edges in the graph
tail_of(simpleGraph,E(simpleGraph))

#
## simplifiedGraph <- simplify(simplifiedGraph)


# 6. Determine the (a) central person(s) in the graph,
# (b) longest path, (c) largest clique 

# 6(a) Central Person(s) in the graph


# Simplified graph aka all data
centr_degree(simplifiedGraph)$centralization
# --The central degree is 1.514154 * 10 ^ -5 

# Closeness Centrality 
closeness(ego)
centr_clo(simplifiedGraph,mode="all")$centralization
# 1.860223 * 10 ^-10

# Eigen Centrality 
eigen_centrality(simplifiedGraph,directed = TRUE,scale = FALSE, weights = E(simplifiedGraph)$weight)
# The most central node in the graph is node # 361652.


# 6(b) Longest path
all_shortest_paths(sg1,from=1,to=vcount(sg1))
average.path.length(sg1)
# The average path length is 1.655172 
average.path.length(simplifiedGraph)
# The average path length is 1.5547

# Check if graph is connected
is_connected(simplifiedGraph)

# The mean distance is 1.549351
mean_distance(simplifiedGraph)



# 6(c) Largest clique 

# Experimenting with cliques function
cliques <- cliques(sg1,min=1)

# Largest cliques stored here in largestCliques variable
largestCliques <- max_cliques(sg1,min=1,max=NULL,subset=NULL,file=NULL)

largestCliques

# The largest clique is of size 3, there are multiple largest cliques

# 6(d) Ego
# ego <- ego(simplifiedGraph)
# ego
ego <- ego.extract(rows)
ego

# 6(e) Betweenness centrality and power centrality 
betweenness <- betweenness(rows)
betweenness 
# The betweenness centrality value is 0.03219316

power_centrality <- power_centrality(sg1, exponent=0.9)
power_centrality

# a. Is there more than one person with the most degrees?
# Yes

# b. Are there multiple longest paths?
# Yes, there are since there are multiple longest paths in multiple
# cliques. 


# c. Are there multiple cliques?
# Yes, there are 3 cliques. 
clique_num(simplifiedGraph)


# d. Are there more than one person(s) with the highest ego?
# Yes
# e. What is the difference in betweenness centrality vs. 
# power centrality for the cases you find? Consider 
# comparing the nodes that are members of each set. 
# Are there common nodes? 

# In each case what do you think the data tells you?

# 7. Find the 20 nodes with the greatest neighborhood out to a 
# distance 3 from the node. DO any of these neighborhoods overlap?
# https://igraph.org/r/doc/communities.html
wc1 <- cluster_walktrap(sg1)
modularity(wc1)
membership(wc1)
sizes(wc1)

plot(wc1,sg1,vertex.size=0.1,layout=layout.fruchterman.reingold,vertex.label=NA)

neighbors(simplifiedGraph,4)
neighbors(simplifiedGraph,100)


for(i in 1:100){
  print(neighbors(simplifiedGraph,i))
}  



# 7a. Build a matrix of 20 nodes with their reachability to the 3rd level

# 7b. Determine which of the 20 nodes share common nodes, if any, and,
# for each common node, list the nodes that share that common node.




