## supercorr.R | v2022.01.18

# Will create a new dataframe containing the correlation matrices, which themselves contain correlations between the identified variables
xyzcompare <- function(df,          # Dataframe to be analyzed
                       wtnvars,     # A list of variable names across which correlations should be run 
                       btwvars)     # A list of variable names across which correlations should not be run
{
  # Dependent packages----
  library(tidyverse)

  wtnvars <- list(Cond, Run)
  btwvars <- list(PID, ROI)
  
  rows <- rows[!str_detect(string = colnames(df),
                           pattern = "_xyz$")]
  
  rows <- str_replace_all(rows,
                          pattern = "_val$",
                          replacement = "")  
  
  cols <- apply(expand.grid(wtnvars[1:length(wtnvars)]), 1, paste, collapse=".")
  
  df_cor <- data.frame(matrix(NA, 
                              nrow = length(rows), 
                              ncol = length(cols), 
                              dimnames = list(rows, cols)))
  
  # Iterate through every value of variable 1
  for (i in 1:length(btwvars[[1]])){
    #Iterate through every value of variable 2
    for (j in 1:length(btwvars[[2]])){
      # Find every instance among the column names where both variable 1 and variable 2 are present
      range <- which(str_detect(colnames(df), pattern = paste0("^", btwvars[[1]][i],"_", btwvars[[2]][j], "_")) == TRUE)
      for (k in 1:length(range)){
        for (m in 1:length(range)){
          if (k>=m)
            df_cor[((e - 1) * length(PIDs)) + i, c] <- NA
          if (k<m)
            df_cor[((e - 1) * length(PIDs)) + i, c] <- cor(as.numeric(df[,((e - 1) * length(PIDs)) + i]), 
                                                           as.numeric(df[,((e - 1) * length(PIDs)) + c]), 
                                                           use ="pairwise.complete.obs", method = "spearman")
          
        }
      }
    }
  }
}
