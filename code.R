rm(list = ls()) #remove the variables from the R environment
#set the workspace
setwd("~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment")

#import the "csv" files created in R
df16 <- read.csv('data2016.csv', header = T)
head(df16, 20)
df17 <- read.csv('data2017.csv', header = T)
head(df17, 20)
df18 <- read.csv('data2018.csv', header = T)
head(df18, 20)
df19 <- read.csv('data2019.csv', header = T)
head(df19, 20)
df20 <- read.csv('data2020.csv', header = T)
head(df20, 20)

#import the libraries we are going to use later on
library(igraph); library(dplyr); library(ggplot2)

###Q-1
#create the graphs, one for each year
g16 <- graph_from_data_frame(df16, directed=FALSE)
g17 <- graph_from_data_frame(df17, directed=FALSE)
g18 <- graph_from_data_frame(df18, directed=FALSE)
g19 <- graph_from_data_frame(df19, directed=FALSE)
g20 <- graph_from_data_frame(df20, directed=FALSE)

#print them
print(g16, e=TRUE, v=TRUE); print(g17, e=TRUE, v=TRUE)
print(g18, e=TRUE, v=TRUE); print(g19, e=TRUE, v=TRUE)
print(g20, e=TRUE, v=TRUE)

#check whether the graphs are directed and then weighted
is.directed(g16); is.directed(g17); is.directed(g18); is.directed(g19); is.directed(g20) 
is.weighted(g16); is.weighted(g17); is.weighted(g18); is.weighted(g19); is.weighted(g20)

allgraphs <- list(g16, g17, g18, g19, g20) #list with all the graphs
d <- c(2016, 2017, 2018, 2019, 2020)

#plot them
pdf(file = 'graphs.pdf', width = 9, height = 6)
  j <- 2016
  for (i in allgraphs) {
    plot(i, vertex.label = NA, edge.arrow.width = 0.8,
       edge.arrow.size = 0.2, vertex.size = 3, main = paste("Graph for year: ", j, sep = ""))
    j <- j+1
  }
dev.off()

###Q-2
#number of vertices
n_vertices_by_graph <- data.frame(date = d, metric = "number_vertices", score = sapply(allgraphs, vcount))
#number of edges
n_edges_by_graph <- data.frame(date = d, metric = "number_edges", score = sapply(allgraphs, ecount))
#diameter of the graph
diameter_by_graph <- data.frame(date = d, metric = "diameter", score = sapply(allgraphs, diameter))
#average degree
avg_degree_by_graph <- data.frame(date = d, metric = "avg_degree", score = sapply(allgraphs, function(x) {
  mean(degree(x, mode = "all"))}))

#plots for every one of the above data-frames
ggplot(n_vertices_by_graph, aes(date, score, color = metric)) + geom_path()                                  
ggplot(n_edges_by_graph, aes(date, score, color = metric)) + geom_path()                                  
ggplot(diameter_by_graph, aes(date, score, color = metric)) + geom_path()                                  
ggplot(avg_degree_by_graph, aes(date, score, color = metric)) + geom_path()                                  

###Q-3
#########DEGREE##########
#create a list with the degrees of every node of each graph
alldegrees <- sapply(allgraphs, degree, mode = "all")
#sort the above elements of the list
l2 <- sapply(alldegrees, function(x) {sort(x, decreasing = T)})
#take the top 10 authors with the biggest degree of every year
l3 <- lapply(l2, head, 10)
#rename the items of the list with the years
l1 <- l3
names(l1) <- d
l1
#extract the names of the above top-10s authors
dg_names <- unique(names(unlist(l3)))
l <- length(dg_names)

#create a data-frame that contains the name of each of the above authors and their degrees for every year
df_degree <- data.frame(name = dg_names, y2016 = numeric(l), y2017 = numeric(l), y2018 = numeric(l), y2019 = numeric(l), y2020 = numeric(l))

dg2016 <- degree(g16, mode = "all"); dg2017 <- degree(g17, mode = "all")
dg2018 <- degree(g18, mode = "all"); dg2019 <- degree(g19, mode = "all"); dg2020 <- degree(g20, mode = "all")

#design a function that gets as arguments a numeric and a character vector "i",
#checks if it is contained in the graph and if yes then returns the value, if not it returns "0"
#this is useful for later steps
q3_function <- function(dg, i){
  if (length(which(names(dg) == i) == 0) == 1){
    return(dg[which(names(dg) == i)])
  } else {
    return(0)
  }
}

j <- 1 #an iterator of each line in the "df_degree" data-frame
#this for loop calls for each one of the above unique names of the authors the "q3_function" and checks
#if this author belongs to the corresponding graph. If yes, then assign to this line (author), his/her degrees  
#for that year
for (i in dg_names) {
  df_degree$y2016[j] <- q3_function(dg2016, i)
  df_degree$y2017[j] <- q3_function(dg2017, i)
  df_degree$y2018[j] <- q3_function(dg2018, i)
  df_degree$y2019[j] <- q3_function(dg2019, i)
  df_degree$y2020[j] <- q3_function(dg2020, i)
  j <- j+1
}

rownames(df_degree) <- df_degree$name #set as index the names of the authors
df_degree$name <- NULL #delete the column because it is duplicated
df_degree #with this data-frame we see the evolution of the top-10 authors across the 5 years

#finally we keep the top-10 authors with the most degrees on average for all the 5 years 
head(sort(rowMeans(df_degree), decreasing = T), 10) 

########PAGE-RANK###########
#we follow the same steps as we did before
all_pageranks <- sapply(allgraphs, function(x) { page_rank(x)$vector})
l2 <- sapply(all_pageranks, function(x) {sort(x, decreasing = T)})
l3 <- lapply(l2, head, 10)
#rename the items of the list with the years
l1 <- l3
names(l1) <- d
l1 #top-10 authors based on pageRank for every year

pg_names <- unique(names(unlist(l3)))
l <- length(pg_names)
df_pagerank <- data.frame(name = pg_names, y2016 = numeric(l), y2017 = numeric(l), y2018 = numeric(l), y2019 = numeric(l), y2020 = numeric(l))

pg16 <- page_rank(g16)$vector
pg17 <- page_rank(g17)$vector
pg18 <- page_rank(g18)$vector
pg19 <- page_rank(g19)$vector
pg20 <- page_rank(g20)$vector

j <- 1
#here, we used again the same "q3_function", but we insert a numeric vector referring to pagerank instead of degrees
for (i in pg_names) {
  df_pagerank$y2016[j] <- q3_function(pg16, i)
  df_pagerank$y2017[j] <- q3_function(pg17, i)
  df_pagerank$y2018[j] <- q3_function(pg18, i)
  df_pagerank$y2019[j] <- q3_function(pg19, i)
  df_pagerank$y2020[j] <- q3_function(pg20, i)
  j <- j+1
}

rownames(df_pagerank) <- df_pagerank$name
df_pagerank$name <- NULL

df_pagerank <- round(df_pagerank, 5)
df_pagerank #this dataframe contains the authors with biggest pagerank and their evolution in the last 5 years

#now, we take the top-10 authors based on their mean pagerank value for the 5 years
head(sort(rowMeans(df_pagerank), decreasing = T), 10)

###Q-4
#examine the clustering algorithms on graphs for every year
communities_infomap <- sapply(allgraphs, cluster_infomap)
communities_louvain <- sapply(allgraphs, cluster_louvain)
communities_fast_greedy <- sapply(allgraphs, cluster_fast_greedy)

#we see here that the last algorithm cannot be computed correctly for all the years
#so, we examine it separately 
communities_fast_greedy16 <- cluster_fast_greedy(g16)
communities_fast_greedy17 <- cluster_fast_greedy(g17)
communities_fast_greedy18 <- cluster_fast_greedy(g18)
communities_fast_greedy19 <- cluster_fast_greedy(g19) #the problem appears in this year
communities_fast_greedy20 <- cluster_fast_greedy(g20)

#get the modularity scores
lapply(communities_infomap, modularity)
lapply(communities_louvain, modularity)
modularity(communities_fast_greedy16); modularity(communities_fast_greedy17); modularity(communities_fast_greedy18); modularity(communities_fast_greedy20)

#we compare the above algorithms and evaluate their performance
compare(cluster_infomap(g16), cluster_louvain(g16))
compare(cluster_infomap(g17), cluster_louvain(g17))
compare(cluster_infomap(g18), cluster_louvain(g18))
compare(cluster_infomap(g19), cluster_louvain(g19))
compare(cluster_infomap(g20), cluster_louvain(g20))

###select louvain clustering, for the following steps
mem <- lapply(communities_louvain, membership)

#creates a table that contains the number that each author appears in the last 5 years
allnames <- table(names(unlist(alldegrees)))
allnames <- allnames[allnames == 5] #we select only those that appear in all the 5 years
head(allnames)

#set the seed in order to have randomly each time the program have been executed the same author
set.seed(10)
user <- names(sample(allnames, 1))

#for that specific author we check the evolution regarding communities across the years
user_com <- data.frame(date = d, metric = "community", belongs_to = sapply(mem, function(x){unlist(x)[which(names(unlist(x)) == user)]}))
user_com

#create a new column for each graph containing the members of their community
for (i in 1:5) {
  allgraphs[[i]]$community <- communities_louvain[[i]]$membership
}

num <- list(1,2,3,4,5) #list with items 1-5
#find the communities that our user belongs to across the years
com <- lapply(num, function(x) {V(allgraphs[[x]])[allgraphs[[x]]$community == user_com[x,3]]})
names(com) <- d
com
#number of communities members each year
lapply(com, length)

#get the combinations of names of list elements in order to find the common neighbors our user had across the years
nms <- combn(names(com), 2, FUN = paste0, collapse = "-", simplify = FALSE)
#make the combinations of list elements
l <- combn(com, 2, simplify = FALSE )
#intersect the list elements and print the outcome
out <- lapply(l, function(x) intersection(x[[1]], x[[2]]))
out <- setNames(out , nms)
out

#extract to pdf plots containing the 5 graphs and with different color their communities
pdf(file = 'communities.pdf', width = 9, height = 6)
for (i in 1:5) {
  k <- allgraphs[[i]]
  j <- communities_louvain[[i]]
  V(k)$color <- factor(membership(j))
  is_crossing <- crossing(k, communities = j)
  E(k)$lty <- ifelse(is_crossing, "solid", "dotted")
  community_size <- sizes(j)
  #we create a sub-graph in order to drop those communities that are either very small or very large
  #we saw that on average small communities are those that contained less than 5 nodes
  #and very large were those that consist of more than 100 nodes
  in_mid_community <- unlist(j[community_size >= 5 & community_size <= 100])
  sub <- induced.subgraph(k, in_mid_community)
  plot(sub, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2,
       coords = layout_with_fr(sub), margin = 0, vertex.size = 4, main = paste("Graph for year: ", d[i], sep = ""))  
}
dev.off()
##THE END!