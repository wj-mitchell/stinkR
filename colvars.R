## colvars.R | v2022.01.18

# Will create array variables with values pulled from columns names.
# Assumes that all column names should be analyzed. Also assumes that
# disparate array values are separated by a character that only serves 
# that specific function.
colvars <- function(df,          # Dataframe to be analyzed
                    vars,        # An array of variable names in the order in which they appear in every column name 
                    split = "_") # The character that separates different variable elements
{
  # Dependent packages----
  library(tidyverse)
  
  # Errors----
  if (length(vars) != length(strsplit(colnames(df)[1],
                                      split = split,
                                      fixed = T)[[1]])){
    stop(paste0("The number of variables you have entered (",
                length(vars),
                " is different than the number of distinct elements detected within the columns names. Please double check your formatting and try again."))
  }
  
  # Split the first column name and find how many elements it has----
  for (i in 1:length(vars)){
  # Create a new variable from the i position in the array vars----
  # Assign to that variable all of the unique elements that can be found from that same
  # position amongst the different column names throughout the dataframe.
    assign(vars[i], unique(unlist(strsplit(colnames(df), 
                                           split = split, 
                                           fixed = T))[seq(i,
                                                           length(colnames(df)) * length(vars),
                                                           length(vars))]))
    return(vars[i])
  }
}