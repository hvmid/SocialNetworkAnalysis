library(igraph)
library(networkD3)

#----------------------------------------- Part I -----------------------------------------------------------------

#----------------------------------------- Reading the books -----------------------------------------------------------------
book1<- as.matrix(read.table("hpbook1.txt"))
names.df<-read.table("hpnames.txt", header=TRUE)
book1.graph<-graph_from_adjacency_matrix(book1, mode="undirected")
V(book1.graph)$name=as.vector(names.df$name)
plot(simplify(book1.graph), layout=layout.fruchterman.reingold(book1.graph, niter=10000), vertex.size=1.5,vertex.label.cex=0.35)
plot(simplify(book1.graph), layout=layout.circle, vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(simplify(book1.graph))

book2<- as.matrix(read.table("hpbook2.txt"))
book2.graph<-graph_from_adjacency_matrix(book2, mode="undirected")
V(book2.graph)$name=as.vector(names.df$name)
plot(simplify(book2.graph), layout=layout.fruchterman.reingold(book2.graph, niter=10000), vertex.size=1.5,vertex.label.cex=0.35)
plot(simplify(book2.graph), layout=layout.circle, vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(simplify(book2.graph))

book3<- as.matrix(read.table("hpbook3.txt"))
book3.graph<-graph_from_adjacency_matrix(book3, mode="undirected")
V(book3.graph)$name=as.vector(names.df$name)
plot(simplify(book3.graph), layout=layout.fruchterman.reingold(book3.graph, niter=10000), vertex.size=1.5,vertex.label.cex=0.35)
plot(simplify(book3.graph), layout=layout.circle,vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(simplify(book3.graph))

book4<- as.matrix(read.table("hpbook4.txt"))
book4.graph<-graph_from_adjacency_matrix(book4, mode="undirected")
V(book4.graph)$name=as.vector(names.df$name)
plot(simplify(book4.graph), layout=layout.fruchterman.reingold(book4.graph, niter=10000), vertex.size=1.5,vertex.label.cex=0.35)
plot(simplify(book4.graph), layout=layout.circle, vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(simplify(book4.graph))

book5<- as.matrix(read.table("hpbook5.txt"))
book5.graph<-graph_from_adjacency_matrix(book5, mode="undirected")
V(book5.graph)$name=as.vector(names.df$name)
plot(simplify(book5.graph), layout=layout.fruchterman.reingold(book5.graph, niter=10000), vertex.size=1.5,vertex.label.cex=0.35)
plot(simplify(book5.graph), layout=layout.circle, vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(simplify(book5.graph))

book6<- as.matrix(read.table("hpbook6.txt"))
book6.graph<-graph_from_adjacency_matrix(book6, mode="undirected") 
V(book6.graph)$name=as.vector(names.df$name)
plot(simplify(book6.graph), layout=layout.fruchterman.reingold(book6.graph, niter=10000), vertex.size=1.5,vertex.label.cex=0.35)
plot(simplify(book6.graph), layout=layout.circle, vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(simplify(book6.graph))

#----------------------------------------- Binding -----------------------------------------------------------------

attrs <- rbind(as_data_frame(book6.graph, "vertices"), as_data_frame(book5.graph, "vertices")) %>% unique()
el <- rbind(as_data_frame(book6.graph), as_data_frame(book5.graph))
new_g <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
V(new_g)$name=as.vector(names.df$name)

attrs <- rbind(as_data_frame(book6.graph, "vertices"), as_data_frame(book5.graph, "vertices")) %>% unique()
el <- rbind(as_data_frame(book4.graph), as_data_frame(new_g))
new_g1 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
V(new_g1)$name=as.vector(names.df$name)


attrs <- rbind(as_data_frame(book6.graph, "vertices"), as_data_frame(book5.graph, "vertices")) %>% unique()
el <- rbind(as_data_frame(book3.graph), as_data_frame(new_g1))
new_g2 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
V(new_g2)$name=as.vector(names.df$name)

attrs <- rbind(as_data_frame(book6.graph, "vertices"), as_data_frame(book5.graph, "vertices")) %>% unique()
el <- rbind(as_data_frame(book2.graph), as_data_frame(new_g2))
new_g3 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
V(new_g3)$name=as.vector(names.df$name)

attrs <- rbind(as_data_frame(book6.graph, "vertices"), as_data_frame(book5.graph, "vertices")) %>% unique()
el <- rbind(as_data_frame(book1.graph), as_data_frame(new_g3))
new_g4 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
V(new_g4)$name=as.vector(names.df$name)
plot(simplify(new_g4), layout=layout.fruchterman.reingold(new_g4, niter=10000), vertex.size=1,vertex.label.cex=0.4, asp =0.5)
plot(simplify(new_g4), layout=layout.circle, vertex.size=1.5,vertex.label.cex=0.35, asp=0.15)
summary(new_g4)

#----------------------------------------- Analysis I -----------------------------------------------------------------

#Centrality Scores
eigen.cent <- eigen_centrality(new_g4)
V(new_g4)$centrality<-eigen.cent$vector

write.csv(V(new_g3)$name, file = "foo.csv")
nodes <- read.csv("foo.csv", header=T)
nodes[, 3]<-V(new_g4)$centrality
dat1<-nodes[order(-nodes$V3),]

#Betweeness Score
nodes[,4]<-betweenness(new_g4)
dat2<-nodes[order(-nodes$V4),]
#Closeness Score
nodes[,5]<-closeness(new_g4)
dat3<-nodes[order(-nodes$V5),]

head(dat1$x)
head(dat2$x)
head(dat3$x)

#----------------------------------------- Part II -----------------------------------------------------------------
#----------------------------------------- HeatMap -----------------------------------------------------------------

add_books = book1+book2+book3+book4+book5+book6
all.graph<-graph_from_adjacency_matrix(add_books, mode="undirected")
V(all.graph)$name=as.vector(names.df$name)

dimnames(add_books) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(add_books, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

dimnames(book1) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book1, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

dimnames(book2) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book2, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

dimnames(book3) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book3, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

dimnames(book4) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book4, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

dimnames(book5) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book5, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

dimnames(book6) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book6, col=grey(1-c(0,seq(0.5,1,length=500))), symm=TRUE)

#----------------------------------------- Analysis II -----------------------------------------------------------------
#Centrality Scores
eigen.cent <- eigen_centrality(all.graph)
V(all.graph)$centrality<-eigen.cent$vector

write.csv(V(new_g3)$name, file = "doo.csv")
nodes2 <- read.csv("doo.csv", header=T)
nodes2[, 3]<-V(all.graph)$centrality
dat4<-nodes2[order(-nodes2$V3),]

#Betweeness Score
nodes2[,4]<-betweenness(all.graph)
dat5<-nodes2[order(-nodes2$V4),]

#Closeness Score
nodes2[,5]<-closeness(all.graph)
dat6<-nodes2[order(-nodes2$V5),]

head(dat4$x)
head(dat5$x)
head(dat6$x)

#----------------------------------------- Part III -----------------------------------------------------------------

char<-read.csv("characters.csv", header=T)
relation<-read.csv("relations.csv", header=T)

graph_alt<-graph_from_data_frame(relation, char, directed = FALSE)

E(graph_alt)$color<-"blue"

for (E in 1:513){
  if(relation[E,3]=="-"){
    E(graph_alt)$color[E]<-"red"
  }
}

plot(simplify(graph_alt), layout.fruchterman.reingold(simplify(graph_alt), niter=1000), vertex.size=1.5, vertex.label.cex=0.35, edge.color=E(graph_alt)$color)

#----------------------------------------- DIFFERENT -----------------------------------------------------------------

el <- data.frame(PARTNER1=relation$source, PARTNER1=relation$target, TYPE=relation$type)

el$COLOR[el$TYPE=="-"] <- "red"

el$COLOR[el$TYPE=="+"] <- "green"

g23 <- graph_from_data_frame(d = el, directed = FALSE)

plot(simplify(g23), layout.fruchterman.reingold(simplify(g23), niter=1000),vertex.size=1.5, vertex.label.cex=0.35, edge.color=edge_attr(g23)$COLOR)

#----------------------------------------- SPLIT -----------------------------------------------------------------
vector = c()
split <- read.csv("doo2.csv", header=T)
for (i in 1:513){
  if(relation[i,3]=="-"){
    vector <- c(vector, i)
  }
}

counter<-1
for (i in vector){
  split[counter, 1]<-relation$source[i]
  split[counter, 2]<-relation$target[i]
  counter<-counter+1
}

vector2 = c()
split2 <- read.csv("doo2.csv", header=T)
for (i in 1:513){
  if(relation[i,3]=="+"){
    vector2 <- c(vector2, i)
  }
}
counter2<-1
for (i in vector2){
  split2[counter2, 1]<-relation$source[i]
  split2[counter2, 2]<-relation$target[i]
  counter2<-counter2+1
}

graph_enemy<-graph_from_data_frame(split, char, directed = FALSE)

graph_ally<-graph_from_data_frame(split2, char, directed = FALSE)

plot(simplify(graph_enemy), layout.fruchterman.reingold(simplify(graph_enemy), niter=1000),vertex.size=1.5, vertex.label.cex=0.35)
plot(simplify(graph_ally), layout.fruchterman.reingold(simplify(graph_ally), niter=1000),vertex.size=1.5, vertex.label.cex=0.35)

#----------------------------------------- Community Detection -----------------------------------------------------------------



