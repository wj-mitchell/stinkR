pacman::p_load(png, grid, gridExtra)
source("C:/Users/Administrator/Desktop/Scripts/stinkR/make_df.R")

# ---- LOADING THE IMAGE ----
setwd("C:/Users/Administrator/Desktop")

# load the PNG into an RGB image object
image = readPNG("test.PNG")

# This image is 512 x 512 x 3 array
dim(image)

# ---- SEGMENTING THE IMAGE ----
# reshape image into a data frame
df = data.frame(
  red = matrix(image[,,1], ncol=1),
  green = matrix(image[,,2], ncol=1),
  blue = matrix(image[,,3], ncol=1)
)

# Create a dataframe to house the average RGB values for each image
df_sum <- make_df(cols = c("Red", "Green", "Blue"),
                  rows = 1)

# ---- CALCULATING MEAN RGB VALUE ----
# Converting scale from 0-1 to 0-255
# Squaring the values (According to https://sighack.com/post/averaging-rgb-colors-the-right-way#:~:text=The%20typical%20approach%20to%20averaging,components%20of%20the%20final%20color.)
df[,] <- (df[,] * 255)^2  

# Calculating the average RGB Value
for (i in 1:ncol(df)){
  df_sum[1,i] <- sqrt(mean(df[,i]))
}

# -----CONVERTING RGB to Luma ----
# https://en.wikipedia.org/wiki/Luma_(video)
# https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
# Creating dataframe
df_luma <- make_df(cols = c("Value"),
                  rows = 1)

# Calculating luma by Digital ITU BT.601 standards
for (i in 1:nrow(df_sum)){
  df_luma$Value[i] <- (0.299*df_sum$Red[i]) + (0.587*df_sum$Green[i]) + (0.114*df_sum$Blue[i])
}

