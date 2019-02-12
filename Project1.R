## Download and install the package
install.packages("igraph")
## Load package
library(igraph)
# Get edges from text file 
edges1 <- read.table("roadNet-CA.txt")
# Convert edges to matrix
em <-as.matrix(edges1)
# Get all the rows of the matrix
rows <- em[1:nrow(em),]
# Show the rows 
rows
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
# Plot graph
plot(g)

