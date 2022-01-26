## removeNAs.R | v2022.01.18

# Will just remove any rows that have all NAs in them
removeNAs <- function(df)
  {
  df[rowSums(is.na(df)) != ncol(df), ]
  return(df)
  }