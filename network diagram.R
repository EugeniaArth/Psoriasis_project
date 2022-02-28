#network diagram for correlation using igraph

forcor <- TransBefore %>% select(-PASI, -BSA, -sPGA)

corrnet <- cor(TransBefore, method = "spearman")
#Keep only high correlations/2 nodes will be connected 
#if their correlation or distance reach a threshold (0.995 here).
corrnet[corrnet<0.2] <- 0
# Make an Igraph object from this matrix:
# Make an Igraph object from this matrix:
network <- graph_from_adjacency_matrix(corrnet, weighted = 'correlation', mode=, diag=F)
# Basic chart
plot(network, layout = layout_in_circle)


nodes <- read.csv("Nodes.csv", head=TRUE,dec = ".", sep=";", stringsAsFactors=FALSE)
links <- read.csv("net.csv", head=TRUE,dec = ".", sep=";", stringsAsFactors=FALSE)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
mean(links$weight)
#отсечка значения r 
cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
net.sp <- delete_edges(net, E(net)[weight<0.5])
#plot
plot(net.sp, layout=layout_with_kk, vertex.size=20, edge.width = 3)
#plot negative/positive associations for all r
E(net)$color[E(net)$type=="negative"] <- 'red'
E(net)$color[E(net)$type=="positive"] <- 'grey'
plot(net,  layout = layout_in_circle)
#plot negative/positive associations if cut off
E(net.sp)$color[E(net.sp)$type=="negative"] <- 'red'
E(net.sp)$color[E(net.sp)$type=="positive"] <- 'grey'
plot(net.sp,  layout = layout_in_circle)
plot(net.sp, layout = layout_in_circle, vertex.size=40)

#plot(net.sp, edge.color=c("dark red", "slategrey")[(E(net)$type=="positive") + 1 ], layout=layout_in_circle)

net_gr <- plot(net.sp, edge.curved=.1) 

