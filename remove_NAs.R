## remove_NAs.R | v2022.01.18

# Will just remove any rows and/or columns that have all NAs in them
remove_NAs <- function(df,
                       rows = TRUE,
                       cols = TRUE,
                       rows_range = NA,
                       cols_range = NA)
  { 
  
  ## Errors ----
  
  ## df is not a dataframe ----
  if (!is.data.frame(df)){
    df <- as.data.frame(df)
    if (!is.data.frame(df)){
      stop(print("df must be formatted as a dataframe"))
    }
  }
  
  ## rows is not a boolean ----
  if (rows != TRUE & rows != FALSE){
    stop(print("rows must take a value of either TRUE or FALSE"))
  }
  
  ## cols is not a boolean ----
  if (cols != TRUE & cols != FALSE){
    stop(print("cols must take a value of either TRUE or FALSE"))
  }
  
  ## rows_range is not an array of positive integers ----
  if (all(!is.na(rows_range))){
    if (all(!is.numeric(rows_range)) | all(rows_range <= 0)){
      stop(print("rows_range must be an array of positive numeric values corresponding to index 
               positions in your target dataframe. Your entry is not a numeric array of positive integrets.
               Please revise your entry and try again."))
    }
  }
  
  ## rows_range is within the boudnaries of df ----
  if (all(!is.na(rows_range))){
    if (rows_range[1] < 1 | rows_range[length(rows_range)] > length(rownames(df))){
      stop(print("rows_range must be an array of positive numeric values corresponding to index 
               positions in your target dataframe. The range of values that you've entered extend 
               beyond the boundaries of the dataframe. Please revise your entry and try again."))
    }
  }
  
  ## cols_range is not an array of positive integers ----
  if (all(!is.na(cols_range))){
    if (any(!is.numeric(cols_range)) | any(cols_range <= 0)){
      stop(print("cols_range must be an array of positive numeric values corresponding to index 
               positions in your target dataframe. Your entry is not a numeric array of positive integrets.
               Please revise your entry and try again."))
    }
  }

  ## cols_range is within the boudnaries of df ----
  if (all(!is.na(cols_range))){
    if (cols_range[1] < 1 | cols_range[length(cols_range)] > length(rownames(df))){
      stop(print("cols_range must be an array of positive numeric values corresponding to index 
                 positions in your target dataframe. The range of values that you've entered extend 
                 beyond the boundaries of the dataframe. Please revise your entry and try again."))
    }
  }
  
## Function ----
  
  ## If Number of NA rows matches the number of total rows, remove -----
  if (rows == TRUE){
    if (any(is.na(cols_range))){
      df <- df[rowSums(is.na(df)) != ncol(df), ]
    }
    if (all(!is.na(cols_range))){
      df <- df[rowSums(is.na(df[,cols_range])) != length(cols_range),]
    }
  }
  
  ## If Number of NA cols matches the number of total cols, remove -----
  if (cols == TRUE){
    if (any(is.na(rows_range))){
      df <- df[,colSums(is.na(df)) != nrow(df)]
    }
    if (all(!is.na(rows_range))){
      df <- df[,colSums(is.na(df[,rows_range])) != length(rows_range)]
    }
  }

  
  return(df)
  
  }