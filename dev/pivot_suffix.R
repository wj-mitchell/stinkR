pivot_suffix <- function(df,
                         sep)
                         # newcol)
  {
  ## Dependencies ----
  library(tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = T)

  ## Errors ----
  ## Dataframe Not Dataframe ----
  if (!is.data.frame(df)){
    df <- as.data.frame(df)
    if (!is.data.frame(df)){
      stop(paste("df must be formatted as a dataframe. Your entry could not be coerced into a dataframe format. 
                 Please restructure your data and try again."))
    }
  }
  
  ## Separator Not Character ----
  if (!is.character(sep)){
    sep <- as.character(sep)
    if (!is.character(sep)){
      stop(paste("sep must be formatted as a character of length 1 or greater. Your entry could not be coerced into character format. Please restructure your entry and try again."))
    }
  }
  
  ## Function ----
  
  df = df_compare
  sep = "_"
  
  ## Acquiring New Column Names from Old Dataframe ----
  cols <- names(df) %>%
          str_replace_all(pattern = paste0(sep,".*"),
                          replacement = "") %>%
          unique()
  
  ## Isolating the Prefix Components from Column Names ----
  elements <- names(df)[grep(sep ,names(df))] %>%
              str_extract(pattern = paste0(sep,".*")) %>%
              unique() 
  
  ## Specifying Number of Rows in New Dataframe ----
  rows <- elements %>%
          length () %>%
          "*"(nrow(df)) %>%
          seq(1,.,1)
  
  ## Creating New Dataframe ----
  df_temp <- make_df(cols = cols,
                     rows = rows)
  
  ## Specifying Columns to Pivot ----
  pivotcols <- which(str_detect(names(df),
                          pattern = sep,
                          negate = FALSE))

  ## Identifying Columns to Unalter ----
  stablecols <- which(str_detect(names(df),
                           pattern = sep,
                           negate = TRUE))
  
  ## Copying Unaltered Columns ----
  for (i in names(df[,stablecols])){
    df_temp[,which(names(df_temp) == i)] <- rep(df[,i], length(elements))
  }
  
  ## Limiting Column Names to the Pivot Columns ----
  cols <- names(df)[pivotcols] %>%
          str_replace_all(pattern = paste0(sep,".*"),
                          replacement = "") %>%
          unique()
  
  ## Building A Function to Simplify For Loop ----
  chunk <- function(data = df,
                    var = i,
                    comp = elements,
                    add){
    ((which(comp == var) - 1) * nrow(data) + add)
  }
  
  # For each of the elements 
  for (i in elements){
    # For each pivot column in the new dataframe
    for (j in cols){
      # Move the data from the old dataframe to the new dataframe and adjust the targetted rows depending upon which element we're pivoting 
      df_temp[chunk(add = 1):chunk(add = nrow(df)),
              # Matching the column in the new dataframe to one of its equivalents in the old dataframe
              which(names(df_temp) == j)] <- as.factor(df[,
                                                # Identifying which column in the old dataframe has the same prefix and suffix.
                                                which(str_detect(names(df), 
                                                                 paste0(j, i)))])
      # Collecting those elements and inserting them into a new column
      df_temp$Pivot[chunk(add = 1):chunk(add = nrow(df))] <- i
    }
  }
  
  # ## Renaming Element Column ----
  # df_temp <- df_temp %>%
  #   rename(newcol=Pivot)
  
  ## Returning Dataframe ----
  return(df_temp)
}