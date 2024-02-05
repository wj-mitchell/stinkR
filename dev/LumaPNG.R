LumaPNG <- function(png)
  
  {
  # Libraries
  pacman::p_load(png, grid, gridExtra, tidyverse)
  
  # load the PNG into an RGB image object
  image = readPNG(png)
  
  # reshape image into a data frame
  df = data.frame(
    red = matrix(image[,,1], ncol=1),
    green = matrix(image[,,2], ncol=1),
    blue = matrix(image[,,3], ncol=1)
  )
  
  # Cleaning our space
  rm(image)
  
  # ---- CALCULATING MEAN RGB VALUE ----
  # Converting scale from 0-1 to 0-255
  # Squaring the values (According to https://sighack.com/post/averaging-rgb-colors-the-right-way#:~:text=The%20typical%20approach%20to%20averaging,components%20of%20the%20final%20color.)
  df[,] <- (df[,] * 255)^2  
  
  # Calculating the average RGB Value
  for (col in 1:ncol(df)){
    df_sum[PNG,(col + 1)] <- sqrt(mean(df[,col]))
  }
  
  # Print out ----
  if (round(((PNG/length(list.files())) * 100),1) %% 10 == 0){
    print(paste("RGB analysis on PNG files is", 
                round(((PNG/length(list.files())) * 100), 1),
                "% complete |", 
                Sys.time()))
  }
  
  # Cleaning our space
  rm(col, df, PNG)
  
}

# -----CONVERTING RGB to Luma ----
# https://en.wikipedia.org/wiki/Luma_(video)
# https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color

# Print out
print(paste("Creating luma structure |", Sys.time()))

# Creating dataframe
df_sum$luma <- NA 

# Calculating luma by Digital ITU BT.601 standards
for (row in 1:nrow(df_sum)){
  df_sum$luma[row] <- (0.299*df_sum$Red[row]) + (0.587*df_sum$Green[row]) + (0.114*df_sum$Blue[row])
  
  # Print out ----
  if (round(((row/nrow(df_sum)) * 100),1) %% 100 == 0){
    print(paste(round(((row/nrow(df_sum)) * 100),1),
                "% of rows converted from RGB to luma |", 
                Sys.time()))
  }
}

# Print out
print(paste("Z-scoring luma values |", Sys.time()))

# Z-scoring luma values
df_sum$luma_z <- as.numeric(scale(df_sum$luma))

# Print out
print(paste("Analysis complete! |", Sys.time()))

}

pacman::p_load(png, grid, gridExtra, tidyverse)
source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R")

# ---- LOADING THE IMAGE ----
setwd(dir)

# Print out
print(paste("Creating the primary dataframe |", Sys.time()))

# Create a dataframe to house the average RGB values for each image
df_sum <- make_df(cols = c("Frame", "Red", "Green", "Blue"),
                  rows = 1:length(list.files()))

# Print out
print(paste("Analyzing PNGs |", Sys.time()))

# Iterate through each of our images
for (PNG in 1:length(list.files())){
  
  # Noting which frame is which
  df_sum$Frame[PNG] <- list.files()[PNG] %>%
                       str_replace(pattern = "\\.png",
                                   replace = "")
  
  # load the PNG into an RGB image object
  image = readPNG(list.files()[PNG])

  # ---- SEGMENTING THE IMAGE ----
  # reshape image into a data frame
  df = data.frame(
    red = matrix(image[,,1], ncol=1),
    green = matrix(image[,,2], ncol=1),
    blue = matrix(image[,,3], ncol=1)
  )
  
  # Cleaning our space
  rm(image)
  
  # ---- CALCULATING MEAN RGB VALUE ----
  # Converting scale from 0-1 to 0-255
  # Squaring the values (According to https://sighack.com/post/averaging-rgb-colors-the-right-way#:~:text=The%20typical%20approach%20to%20averaging,components%20of%20the%20final%20color.)
  df[,] <- (df[,] * 255)^2  
  
  # Calculating the average RGB Value
  for (col in 1:ncol(df)){
    df_sum[PNG,(col + 1)] <- sqrt(mean(df[,col]))
  }
  
  # Print out ----
  if (round(((PNG/length(list.files())) * 100),1) %% 10 == 0){
    print(paste("RGB analysis on PNG files is", 
                round(((PNG/length(list.files())) * 100), 1),
                "% complete |", 
                Sys.time()))
  }
  
  # Cleaning our space
  rm(col, df, PNG)

}

# -----CONVERTING RGB to Luma ----
# https://en.wikipedia.org/wiki/Luma_(video)
# https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color

# Print out
print(paste("Creating luma structure |", Sys.time()))

# Creating dataframe
df_sum$luma <- NA 

# Calculating luma by Digital ITU BT.601 standards
for (row in 1:nrow(df_sum)){
  df_sum$luma[row] <- (0.299*df_sum$Red[row]) + (0.587*df_sum$Green[row]) + (0.114*df_sum$Blue[row])
  
  # Print out ----
  if (round(((row/nrow(df_sum)) * 100),1) %% 100 == 0){
    print(paste(round(((row/nrow(df_sum)) * 100),1),
                "% of rows converted from RGB to luma |", 
                Sys.time()))
  }
}

# Print out
print(paste("Z-scoring luma values |", Sys.time()))

# Z-scoring luma values
df_sum$luma_z <- as.numeric(scale(df_sum$luma))

# Print out
print(paste("Analysis complete! |", Sys.time()))


