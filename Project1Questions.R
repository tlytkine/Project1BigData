## Load packages
library(igraph)
library(sna)

# Get edges from text file, creates data frame 
edges1 <- read.table("roadNet-CA.txt")


# Convert edges to matrix
em <-as.matrix(edges1)

# All rows of data 
allRows <- em[0:5533214,]

# Density of rows: 0.7083459
gden(allRows)

# Get vertices from first column 
allV1 <- allRows[0:5533214,1]

# Get vertices from second column
allV2 <- allRows[0:5533214,2]

# Get Relations 
allRelations <- data.frame(from=allV1,to=allV2)

# Get graph data frame 
allG <- graph.data.frame(allRelations,directed=TRUE)
simpAll <- allG 

# Put weight of edges into E(simpAll)$weight 
E(simpAll)$weight <- rnorm(ecount(simpAll))
E(simpAll)$weight

# Put weight of vertices into V(simpAll)$weight 
V(simpAll)$weight<-rnorm(vcount(simpAll)) 
V(simpAll)$weight 

# Check if graph is simple 
is.simple(simpAll)

# Vertex count of simpAll
vcount(simpAll)

# Edge count of simpAll
ecount(simpAll)

# Simplify graph 
sg9 <- induced.subgraph(simpAll,which(V(simpAll)$weight > 0.9, E(simpAll)$weight > 0.9))
sg9
vcount(sg9)
# sg9 v count = 362127
ecount(sg9)
# sg9 e count = 187172

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




