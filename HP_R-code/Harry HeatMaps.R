library(igraph)

book1<- as.matrix(read.table("hpbook1.txt"))
book2<- as.matrix(read.table("hpbook2.txt"))
book3<- as.matrix(read.table("hpbook3.txt"))
book4<- as.matrix(read.table("hpbook4.txt"))
book5<- as.matrix(read.table("hpbook5.txt"))
book6<- as.matrix(read.table("hpbook6.txt"))
names.df<-read.table("hpnames.txt", header=TRUE)

dimnames(book1) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book1, symm=TRUE, col=gray.colors(10), keep.dendro = FALSE)

book12<-book1+book2

dimnames(book12) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book12, symm=TRUE, col=gray.colors(10), keep.dendro = FALSE)

book123<-book12+book3

dimnames(book123) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book123, symm=TRUE, col=gray.colors(10), keep.dendro = FALSE)

book124<-book123+book4

dimnames(book124) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book124, symm=TRUE, col=gray.colors(10), keep.dendro = FALSE)

book125<-book124+book5

dimnames(book125) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(book125, symm=TRUE, col=gray.colors(10), keep.dendro = FALSE)

bookAll<-book125+book6

dimnames(bookAll) <- list(as.vector(names.df$name), as.vector(names.df$name))
heatmap(bookAll, symm=TRUE, col=gray.colors(10), keep.dendro = FALSE)
