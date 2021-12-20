rm(list = ls()) #remove the variables from the R enviroment
#set the workspace
setwd("~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment")

#import the "csv" files created in Unix Tools
#we observed through MS Excel that some records contained more than 20 authors
#if we had not initialized the number of columns, then some records that had many authors would have placed in more than 2 rows, something which is wrong
auth16 <- read.csv('auth2016.csv', header = F, sep = ",", col.names = paste0("V",seq_len(25)))
head(auth16, 20)
auth17 <- read.csv('auth2017.csv', header = F, sep = ",", col.names = paste0("V",seq_len(25)))
head(auth17, 20)
auth18 <- read.csv('auth2018.csv', header = F, sep = ",", col.names = paste0("V",seq_len(25)))
head(auth18, 20)
auth19 <- read.csv('auth2019.csv', header = F, sep = ",", col.names = paste0("V",seq_len(25)))
head(auth19, 20)
auth20 <- read.csv('auth2020.csv', header = F, sep = ",", col.names = paste0("V",seq_len(25)))
head(auth20, 20)

#now, we read those columns that do not contain any item
#so, they contain only NA values
n16 <- names(auth16)[sapply(auth16, function(x) sum(is.na(x)) == length(x))]
n17 <- names(auth17)[sapply(auth17, function(x) sum(is.na(x)) == length(x))]
n18 <- names(auth18)[sapply(auth18, function(x) sum(is.na(x)) == length(x))]
n19 <- names(auth19)[sapply(auth19, function(x) sum(is.na(x)) == length(x))]
n20 <- names(auth20)[sapply(auth20, function(x) sum(is.na(x)) == length(x))]

#we delete those columns
for (i in n16) { auth16[i] <- NULL }
for (i in n17) { auth17[i] <- NULL }
for (i in n18) { auth18[i] <- NULL }
for (i in n19) { auth19[i] <- NULL }
for (i in n20) { auth20[i] <- NULL }

#we also observed that some rows contained only one author
#these records are useless in creating graphs, because they do not have any pair
auth16 <- auth16[!auth16[[2]]== "",]
auth17 <- auth17[!auth17[[2]]== "",]
auth18 <- auth18[!auth18[[2]]== "",]
auth19 <- auth19[!auth19[[2]]== "",]
auth20 <- auth20[!auth20[[2]]== "",]

#now, we designed a function that creates and returns the wanted format for each year (author1, author2, their_weight) as data-frame
#the function takes as argument the data-frames created earlier
#the main concept of this function is that from the records we have for each year referring to 5 conferences,
#we should extract the pairs of authors that have been cooperated to each other in order to present a paper.
#so, for each row we take all the elements and create pair with their neighbors, counting in parallel the times these authors have been collaborated
auth_fun <- function(auth) {
  #initialize the data-frame we want to export and insert the first record in it
  df <- data.frame(from = character(), to = character(), weight = numeric())
  df[nrow(df) + 1,1] <- auth[1,1] 
  df[nrow(df),2] <- auth[1,2]
  df[nrow(df),3] <- 1
  
  for (i in 1:nrow(auth)) { #iterate the whole data-frame
    for (j in 1:ncol(auth)) { #iterate the columns of each row
      #if the specific cell is null or we are in the end of this row, then go to the next one,
      #because we cannot compare the last value with anyone else
      if (auth[i,j] == "" | j == ncol(auth)){ 
        break
      } else {
        rec = auth[i,j] #save to variable "rec" the element (author) we examine each time
        #"k" checks every element which is located in the same row with "rec" and is placed next to it
        for (k in (j+1):ncol(auth)) { 
          if (auth[i,k] == ""){ #if the specific element is null we go to next j (column)
            break
          } else {
            #if this author (rec) is already have been inserted in the data-frame "df" (once or even more) and is located in its first column,
            #then we will find it with the next command
            if (length(which(grepl(rec, df[,1]))) >= 1) {
              m <- which(grepl(rec, df[,1])) #save the rows where this author (rec) is located in the data-frame"df" in a vector
              for (p in 1:length(m)) { #iterate the vector "m"
                #if the element next to this record is the same with the 2nd element in the column of the "df",
                #this means that this pair of authors has already inserted, so we just need an iteration of their weight
                if (df[m[p],2] == auth[i,k]){ 
                  df[m[p],3] <- df[m[p],3] + 1
                  break
                #if we have reached in the end of the "m" vector and the records we referred earlier are not the same,
                #then we have to insert this pair of authors to the data-frame
                } else if (p == length(m) & df[m[p],2] != auth[i,k]){
                  df[nrow(df) + 1,1] <- rec
                  df[nrow(df),2] <- auth[i,k]
                  df[nrow(df),3] <- 1
                }
              }
            #we repeat the same procedure, but in this time we check if the author is located in the second column in the "df"
            } else if (length(which(grepl(rec, df[,2]))) >= 1) { 
              m <- which(grepl(rec, df[,2]))
              for (p in 1:length(m)) {
                if (df[m[p],1] == auth[i,k]){
                  df[m[p],3] <- df[m[p],3] + 1
                  break
                } else if (p == length(m) & df[m[p],1] != auth[i,k]){
                  df[nrow(df) + 1,1] <- rec
                  df[nrow(df),2] <- auth[i,k]
                  df[nrow(df),3] <- 1
                }
              }
            } else { #in this part the specific author is not inserted in the data-frame, so we create a record for him and his next to him author
              df[nrow(df) + 1,1] <- rec
              df[nrow(df),2] <- auth[i,k]
              df[nrow(df),3] <- 1
            }
          }
        }
      }
    }
  }
  #after the execution of the above for loops, we end up having a data-frame that contains all the pairs of authors that have been 
  #collaborated to each other, in order to complete the paper
  #in the above data-frame we observed that some pair of authors have been inserted twice.
  #this happened because one author may have been added in the first column and in the second too, having the same co-author
  #e.g. (author1, author2,4) & (author2, author1, 1)
  #so, we have to merge these elements and add their weights.
  i <- 1
  while (i <= nrow(df)) {
    m <- which(grepl(df[i,2], df[,1]))
    if (length(m) >= 1){
      p <- c() #"p" is a vector containing all the values of an author that his/her pairs are repeated
      for (j in 1:length(m)) {
        from <- df[m[j],1]
        to <- df[m[j], 2]
        #with the next if-else statement we find those records that the referring phenomenon is happening
        if (dim(df[(df$from == to & df$to == from),])[1] == 0){
          next
        } else {
          p <- c(p, m[j])
          df[(df$from == to & df$to == from),3] <- df[(df$from == to & df$to == from),3] + df[m[j],3]
        }
      }
      #we use p in order to change the dimension of the "df" and remove the duplicates pairs from it
      if (!is.null(p)){
        df <- df[-p,]
      }
    }
    i <- i+1
  }
  return(df) #return the wanted data-frame
}
#the final data-frames
df16 <- auth_fun(auth16) 
df17 <- auth_fun(auth17)
df18 <- auth_fun(auth18)
df19 <- auth_fun(auth19)
df20 <- auth_fun(auth20)

#check for duplicates
#creates data-frames for same of the above data-sets having as first column the second of the data-frame they are referring to
#and as second column their first respectively
df1 <- data.frame(from = df16$to, to = df16$from, weight = df16$weight)
df2 <- data.frame(from = df18$to, to = df18$from, weight = df18$weight)
df3 <- data.frame(from = df20$to, to = df20$from, weight = df20$weight)

#now, we examine if their intersection is null, something that is true
library(dplyr)
inner1 <- inner_join(df1, df16); inner2 <- inner_join(df2, df18); inner3 <- inner_join(df3, df20)

dim(inner1)[1] == 0; dim(inner2)[1] == 0; dim(inner3)[1] == 0

#create the wanted csv files
write.csv(df16,"~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment/data2016.csv", row.names = FALSE)
write.csv(df17,"~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment/data2017.csv", row.names = FALSE)
write.csv(df18,"~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment/data2018.csv", row.names = FALSE)
write.csv(df19,"~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment/data2019.csv", row.names = FALSE)
write.csv(df20,"~/MScBA/4. Spring Semester I/Social Network Analysis/0. Assignments/2nd Assignment/data2020.csv", row.names = FALSE)