# clear all objects 
rm(list=ls())

library(igraph)
library(ergm)
library(intergraph)
library(statnet)
library(network)
library(influenceR)

# setwd 
# read data 
df <- read.csv("EU_Banking_Network.csv",
               stringsAsFactors = F,
               header = T)


# drop last rows which have NAs
df<-df[1:22,]

# assign row names 
rownames(df) <- df$X

# drop the first column 
df <- df[,2:23]

# convert dataframe to a matrix 
mat <- as.matrix(df)

# replace NAs in the matrix with 0s
mat[is.na(mat)]<-0

# create network object 
g <- igraph::graph_from_adjacency_matrix(mat)

#
get.vertex.attribute(g, 'name', index=V(g))

# Plot Network 

# MDS layout used 
l <- layout_with_mds(g)

pdf("g1.pdf",10,10)
plot(g, layout=l, 
     vertex.size=10, 
     edge.arrow.size=.3, 
     vertex.label.dist = 0,
     vertex.label.cex=.7)
dev.off()

# Calculate degree distributions 
d1 <- igraph::degree(g, mode = 'in')
d2 <- igraph::degree(g, mode = 'out')
d3 <- igraph::degree(g, mode = 'all')


### Plot degree distributions ####
pdf("d1.pdf",10,8)
par(cex.lab=1.5)
hist(d1, xlab = "degree", ylab = "frequency", 
     main= "Distribution of in degrees",
     col = 'grey', 
     cex.axis=1.5,
     cex = 2)
dev.off()

pdf("d2.pdf",10,8)
par(cex.lab=1.5)
hist(d2, xlab = "degree", ylab = "frequency", 
     main= "Distribution of out degrees",
     col = 'grey', 
     cex.axis=1.5,
     cex = 2)
dev.off()


pdf("d3.pdf",10,8)
par(cex.lab=1.5)
hist(d3, xlab = "degree", ylab = "frequency", 
     main= "Distribution of all (in & out) degrees",
     col = 'grey', 
     cex.axis=1.5,
     cex = 2)

dev.off()

#################        

# calculate core group density (1st 9 nodes in the block adjacency matrix)
g1 <- igraph::graph_from_adjacency_matrix(mat[1:9,1:9])
edge_density(g1, loops = FALSE)

# calculate periphery group density (remaining nodes)
g2 <- igraph::graph_from_adjacency_matrix(mat[10:22, 10:22])
edge_density(g2, loops = FALSE)

# plot the network 

# Using the ergm library to fit a P1 log-linear probabilistic model 

# edges - no. of edges
# sender - number of out ties of the node (activity of the node)
# receiver - number of in ties of the node (popularity of the node)
# mutual - no. of mutual edges (a->b; b->a) (mutuality)

gx <- as.network(mat,
           directed = T, 
           loops = F,
           matrix.type = 'adjacency')

network.vertex.names(gx) <- rownames(df)

P1 <- ergm(gx ~ edges+sender+receiver+mutual, verbose = TRUE,
           control=control.ergm(seed=123))

summary(P1)


# Brokerage roles 

# convert igraph to an sna object
net <- asNetwork(g)
attr1 <- c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
brokerage(net,attr1)


