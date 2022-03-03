df <- as.data.frame(matrix(data = c(paste0(LETTERS[1],LETTERS[1:24]),
                                    sample(1:20, 24, replace = T),
                                    paste0(LETTERS[2],LETTERS[1:24]),
                                    sample(1:20, 24, replace = T)),
                           nrow = 24,
                           ncol = 4,
                           dimnames = list(1:24, c("G1", "G1Val", "G2", "G2Val"))))

condition_maker <- function(data, # Data frame containing the data
                            items, # An array of column names as strings containing unique identifiers
                            values, # An array of column names names as strings containing values to be added and compared
                            threshold){ # The theshold past which the sum of the values must pass to be included
  ## Dependencies ----
  library(tidyverse)

  item1 <- which(names(data) == items[1])
  item2 <- which(names(data) == items[2])
  value1 <- which(names(data) == values[1])
  value2 <- which(names(data) == values[2])
  
  options <- as.data.frame(matrix(data = NA,
                             nrow = 1,
                             ncol = 9,
                             dimnames = list(1, c("G1R1", "G1R2", "G1R3", "G1R4",
                                                  "G1R5", "G1R6", "G1R7", "G1R8",
                                                  "Total_Time"))))
  
  data[,value1] <- as.numeric(data[,value1])
  data[,value2] <- as.numeric(data[,value2])
  
  for (i in 1:nrow(data)){
    print(paste(round((i/nrow(data)* 100),digits = 0), "% complete | ", Sys.time()))
    for (j in 2:nrow(data)){
      for (k in 3:nrow(data)){
        for (m in 4:nrow(data)){
          for (n in 1:nrow(data)){
            for (o in 2:nrow(data)){
              for (p in 3:nrow(data)){
                for (q in 4:nrow(data)){
                  if (i != j & i!= k & i !=m & j != k & j !=m & k != m){
                    if (n != o & n!= p & n != q & o != p & o != q & p != q){
                      if ((data[i,value1] + data[j,value1] + data[k,value1] + data[m,value1] +
                           data[n,value2] + data[o,value2] + data[p,value2] + data[q,value2]) <= threshold){
                        if (is.na(options[1,1])){
                          options[1,1] <- data[i,item1]
                          options[1,2] <- data[j,item1]
                          options[1,3] <- data[k,item1]
                          options[1,4] <- data[l,item1]
                          options[1,5] <- data[m,item2]
                          options[1,6] <- data[n,item2]
                          options[1,7] <- data[o,item2]
                          options[1,8] <- data[p,item2]
                          options[1,9] <- (data[i,value1] + data[j,value1] + data[k,value1] + data[m,value1] +
                                             data[n,value2] + data[o,value2] + data[p,value2] + data[q,value2])
                        }
                        if (!is.na(options[1,1])){
                          append(options, NA)
                          options[nrow(options),1] <- data[i,item1]
                          options[nrow(options),2] <- data[j,item1]
                          options[nrow(options),3] <- data[k,item1]
                          options[nrow(options),4] <- data[l,item1]
                          options[nrow(options),5] <- data[m,item2]
                          options[nrow(options),6] <- data[n,item2]
                          options[nrow(options),7] <- data[o,item2]
                          options[nrow(options),8] <- data[p,item2]
                          options[nrow(options),9] <- (data[i,value1] + data[j,value1] + data[k,value1] + data[m,value1] +
                                             data[n,value2] + data[o,value2] + data[p,value2] + data[q,value2])
                          
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(options)
}

# Example
(Options <- condition_maker(data = df,
                            threshold = 30,
                            items = c("G1", "G2"),
                            values = c("G1Val", "G2Val"))) 
