kappa = function(M) (sum(diag(M)) - 1/sum(M) * sum(colSums(M) * rowSums(M)))/
  (sum(M) - 1/sum(M) * sum(colSums(M) * rowSums(M)))

df <- make_df(1:2,
              1:20)

df$X1 <- c("AD", "AD", "RM & REAPP", "", "AD",
           "", "AD", "AD & REAPP", "", "AD",
           "AD", "RM", "", "", "RM", 
           "REAPP", "AD", "", "RM & REAPP", "AD")

df$X2 <- c("AD", "AD", "", "AD", "AD",
           "", "AD", "REAPP", "", "AD",
           "", "", "", "", "RM", 
           "REAPP", "AD", "AD", "REAPP", "AD")


col1 <- sample(iris$Species,150L)
col2 <- iris$Species
as.data.frame(rbind(col1,col2))

(sum(diag(table(col1,col2))) - 1/sum(table(col1,col2)) * sum(colSums(table(col1,col2)) * rowSums(table(col1,col2)))) / 
  (sum(table(col1,col2)) - 1/sum(table(col1,col2)) * sum(colSums(table(col1,col2)) * rowSums(table(col1,col2))))



Number of times raters agree - 1/Number of ratings total *
