## make_df.R | v2022.02.02

# Just a simple way to make a quick dataframe when arrays
# for columns and rows are specified; populates as NAs
make_df <- function(cols,
                    rows)
{
  df <- data.frame(matrix(NA, 
                          nrow = length(rows), 
                          ncol = length(cols), 
                          dimnames = list(rows, cols)))
  
  return(df)
}