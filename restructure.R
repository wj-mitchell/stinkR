## restructure.R | v2022.02.02

# Will break components of a file name or an array that are consistently separated 
# by a given character and reassemble those components into a new array that contains
# all possible combinations of those components. It will also create new subcolumns.
# Especially helpful when data is missing. Rather than data files simply being absent 
# from a final product, spaces for missing data will be made and can be filled with NAs.

restructure <- function(array = NA, # An array containing the strings to restructure; can be used instead of directory
                        directory = NA, # A directory containing the filenames that can be turned into strings and restructured; can be used instead of array
                        recursive = TRUE, # If directory is used, this will determine where we are interested in files contained within subdirectories within that file
                        filetype = "csv", # If directory is used, what type of files are we pulling.
                        sep = "_", # The character separating the different components
                        subcols = NA) # An optional array that will replicate every column according to the arrays length and append the elements of the array to the end of each column name. Helpful if working with neural data wherein each datapoint has 3 dimensions and a value.)

{ ## Package Loading ----
  pacman::p_load(assertthat, tidyverse)
  
  ## Options ----
  options(scipen=100)
  options(digits=3)
  options(contrasts = c("contr.helmert", "contr.poly"))
  options(warn = -1)
  
  ## Initial Errors ----
  if (!is.string(directory) & !is.na(directory)){
    stop(print("directory must be entered as a string. Please update directory to comply."))
  }

  if ((!is.na(array) & !is.na(directory)) | (is.na(array) & is.na(directory))){
    stop(print("Please enter either an array of column names or a directory of filenames from which an array can be pulled, but not both."))
  }

  if (recursive != FALSE & recursive != TRUE){
    stop(print("recursive notes whether the files that we're interested in are
               contained within the directoryectory you noted or within a subdirectoryectory.
               It must take a value of FALSE or TRUE, respectively."))
  }
  
  ## If pulling raw filenames ----
  if (!is.na(directory)){
    
  ## Removing Prefixes and Suffices -----
    input <- list.files(path = directory, 
                         recursive = recursive) %>%
      str_replace_all(pattern = paste0("\\.", filetype, "$"), 
                      replacement = "") %>%
      str_replace_all(pattern = "^.*\\/", 
                      replacement = "") %>%
      str_replace_all(pattern = "^.*\\-|^.*\\.", 
                      replacement = "") %>%
      str_split(pattern = sep)
  }
  
  ## 
  if (!is.na(array)){
    
  ## Removing Prefixes and Suffices -----
    input <- array %>%
      str_replace_all(pattern = paste0("\\.", filetype, "$"), 
                      replacement = "") %>%
      str_replace_all(pattern = "^.*\\/", 
                      replacement = "") %>%
      str_replace_all(pattern = "^.*\\-|^.*\\.", 
                      replacement = "") %>%
      str_split(pattern = sep)
  }
  
  ## Isolating Filename Components ----    
    for (i in 1:length(input[[1]])){
      assign(LETTERS[i], unique(sapply(input, "[[", i)))
    }
  
  ## If No Additional Columns Are Needed----
  if (is.na(subcols)){
    
    ## Reassembling the Components into All Possible Combinations ----
    output <- get(LETTERS[1])
    for (i in 2:length(input[[1]])){
      output <- sort(apply(expand.grid(output, get(LETTERS[i])), 1, paste, collapse="_"))
    }              
  }
   
  ## If Additional Columns Are Needed----   
  if (!is.na(subcols)){
    
    ## Reassembling the Components into All Possible Combinations ----    
    output <- get(LETTERS[1])
    for (i in 2:length(input[[1]])){
      output <- apply(expand.grid(output, get(LETTERS[i])), 1, paste, collapse="_")
    }              
    
    ## Adding Additional Columns -----
    output <- output %>% 
              rep(length(subcols)) %>%
              sort() %>%
              paste(., subcols, sep = "_")
  }

  return(output)

}