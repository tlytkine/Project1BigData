## Download and install the package
install.packages("igraph")

## Load package
library(igraph)

edges1 <- read.table("roadNet-CA.txt")
em <-as.matrix(edges1)

rows <- em[1:nrow(em),]
rows

v1 <- rows[0:1000,1]
v2 <- rows[0:1000,2]
v1
v2

relations <- data.frame(from=v1,to=v2)  


g <- graph.data.frame(relations,directed=TRUE)
plot(g)
