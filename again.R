## again.R function | v1.0 2022.01.02

# This function will run any given function whose output is a value or vector 
# nrep times and concatenate the values or vectors into a single vector.

# I primarily use it when running certain sampling functions, which would 
# otherwise produce an array of the same values of nrep length. 

again <- function(func,
                  arg,
                  nrep)
  {
  vector = NA
  for (i in 1:nrep){
    vector <- c(vector, func(arg))
  }
  vector <- vector[-1]
  return(vector)
}