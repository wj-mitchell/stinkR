## paste_all.R | v2022.02.25

# I hadn't developed this solution, but I always forget it, so I wanted to 
# formalize it for myself so that I wouldn't search it all the time. It just 
# allows you to submit a list of arrays and produce a single array combining 
# all elements of the different arrays.
paste_all <- function(list,
                      sep = "_")
{
  # Errors----
  
  
  ## Function ----
  # Combining all elements of the different arrays ----
  array <- apply(expand.grid(list), 
                 1, 
                 paste, 
                 collapse = sep)
  
  return(array)
}