joys_function <- function(filepath,
                          sheet){
  ## Dependent Functions ----
  ### Canonical Packages ----
  library(xlsx)
  library(tidyverse)
  
  ### Custom Functions ----
  # make_df
  make_df <- function(cols,
                      rows)
  {df <- data.frame(matrix(NA, 
                            nrow = length(rows), 
                            ncol = length(cols), 
                            dimnames = list(rows, cols)))
    
    return(df)
  }
  
  # cell_color
  cell_color <- function(style){
    fg  <- style$getFillForegroundXSSFColor()
    
    hex <- tryCatch(fg$getRgb(), error = function(e) NULL)
    hex <- paste0("#", paste(hex, collapse = ""))
    tint <- tryCatch(fg$getTint(), error = function(e) NULL)
    
    if(!is.null(tint) & !is.null(hex)){   # Tint varies between -1 (dark) and 1 (light)
      rgb_col <- col2rgb(col = hex)
      
      if(tint < 0) rgb_col <- (1-abs(tint))*rgb_col
      if(tint > 0) rgb_col <- rgb_col + (255-rgb_col)*tint
      
      hex <- rgb(red = rgb_col[1, 1], 
                 green = rgb_col[2, 1], 
                 blue = rgb_col[3, 1], 
                 maxColorValue = 255)
    }
    
    return(hex)
  }
  
  ## Function Start ----
  ## Loading File ----
  wb <- loadWorkbook(filepath)
  
  ## Identifying Target Sheet ----
  sheet1 <- getSheets(wb)[[sheet]]
  
  ## Identifying Number of Rows ----
  rows  <- getRows(sheet1)
  
  ## Identifying Number of Cells ----
  cells <- getCells(rows)
  
  ## Identifying the Cell ----
  styles <- sapply(cells, getCellStyle)
  
  ## Using the Cell_Color Function ----
  array <- sapply(styles, cell_color)
  
  ## Specifying the Number of Columns in the Original Dataframe ----
  cols <-1:(length(cells)/length(rows))
  
  ## Specifying the Number of Rows in the Original Dataframe ----
  rows <- 1:length(rows)
  
  ## Creating a New Blank Dataframe to House the Values ----
  df <- make_df(rows = rows,
                cols = cols)
  
  ## Copying Values Over ----
  for (i in 1:length(array)){
    if (i%%(length(cols)) != 0){
      df[ceiling(i/length(cols)), i%%(length(cols))] <- array[i]    
    }
    if (i%%(length(cols)) == 0){
      df[ceiling(i/length(cols)), length(cols)] <- array[i]    
    }
  }
  return(df)
}

df <- joys_function(filepath = "C:/Users/Administrator/Downloads/example.xlsx",
                    sheet = 1)
