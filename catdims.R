## catdims.R | v2022.01.18

# Will concatenate x, y, and z dimensions from neuro data into singular columns
# across a wide-formatted dataframe and remove superfluous columns. Coordinates
# must be in column headers and at the end of the name, like: "_x", "_y", "_z".
catdims <- function(df)
{
  
  # Dependent functions----
  library(tidyverse)
  
  # Identifying x columns----
  xcols <- str_detect(colnames(df),
                      pattern = "_x$")
  
  # Identifying y and z columns----
  yzcols <- str_detect(colnames(df),
                       pattern = "_y$|_z$")
  
  # Concatenating values from x, y, and z columns into x column across each row and across whole dataframe.----
  # Assumes y column always follows x column and z always follows y column.
  for (i in 1:length(xcols)){
    if (xcols[i] == TRUE){
      df[,i] <- paste(df[,i], 
                      df[,i + 1],
                      df[,i + 2],
                      sep = ",")
    }
  }
  
  # Changes the name of x column to reflect the fact that it now contains all coordinates----
  colnames(df) <- str_replace_all(string = colnames(df),
                                       pattern = "_x$", 
                                       replacement = "_xyz")
  
  # Removes superfluous y and z columns----
  df <- df[,-which(yzcols == TRUE)]
  
  return(df)
}