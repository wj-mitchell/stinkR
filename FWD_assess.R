library(assertthat)
library(dplyr)
library(stringr)
source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = T)

# Identifying where the data exists
path <- "S:/Helion_Group/studies/rsa_moral/data/deriv/"

# Identifying the different participant IDs
pts <- list.files(path = paste0(path,"FWD_export")) %>%
  str_extract(pattern = "SC0....|CU1....") %>% 
  subset(!is.na(.)) %>%
  unique()

# Naming the number of runs
runs <- paste0("run-",1:5)

# Creating a dataframe to house the percentage of FWD for each participant
df_FWD <- make_df(rows = 1:(length(pts) * length(runs)),
                  cols = c("PTS", "Run", "FWD_rate"))

# Loading pt IDs
df_FWD$PTS <- rep(pts, length(runs)) %>%
              sort()

# Loading runs
df_FWD$Run <- rep(runs, length(pts))

# Iterating through each of the participants
for (i in pts){
  
  # Iterate through the runs in the study
  for (j in runs){
    
    # Create a variable to capture the file name for this run
    filename <- paste0(path, "FWD_export/sub-", i, "_ses-1_task-Aver_", j,"_desc_confounds_FWD.txt")
    
    # Check whether this file exists, and if it doesn't, print an error.
    if (!file.exists(filename)){
      print(paste("Error:", i, "'s", j, "file could not be located. Generating a replacement..."))
      
      # Pull the last dataframe and change all datapoints to 0
      df[,] <- 0 
      
      # And then save that dataframe as a new text file for that participant
      write.table(df, 
                  file = filename, 
                  sep = "\t",
                  row.names = FALSE,
                  col.names = FALSE)
      
      # Note that the datafile has been created
      print(paste("A replacement for", i, "'s", j, "file has been generated!"))
    }
    
    # If it does exist . . . 
    if (file.exists(filename)){
      
      # Check if the file is empty
      if (file.info(filename)$size <= 0){
        print(paste("Error:", i,"'s ", j,"file does not contain data"))
      }
      
      # And if the file isn't empty . . .
      if (file.info(filename)$size > 0){
        
        # Read in the file as a dataframe
        df <- read.delim(file = filename,
                         sep = "",
                         header = T) 
        
        df$row <- 0
        for (k in 1:length(rownames(df))){
          if (any(df[k,] == 1)){
            df$row[k] <- 1
          }
        }
        
        sum(df$row)
        df_FWD$FWD_rate[df_FWD$PTS == i & df_FWD$Run == j] <- (length(rownames(df)) - sum(df$row))/length(rownames(df))
        
      }
    }
  }
}

        