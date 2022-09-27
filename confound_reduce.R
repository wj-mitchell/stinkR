# confound_reducer <- function(path,
#                              confounds,
#                              threshold,){}

# Dependencies
library(assertthat)
library(dplyr)
library(stringr)
source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = T)

# Identifying where the data exists
path <- "S:/Helion_Group/studies/rsa_moral/data/deriv/"

# List of confounds
confounds <- c("cosine00", "csf","white_matter", "framewise_displacement")

# Identifying our threshold
threshold <- 0.9

# Creating a single master directory to host all of the new txt files
dir.create(paste0(path,"confounds_reduced"))

# Identifying the different participant IDs
pts <- list.files(path = paste0(path,"confound_export")) %>%
       str_extract(pattern = "SC0....|CU1....") %>% 
       subset(!is.na(.)) %>%
       unique()

# Iterating through each of the participants
for (i in pts){
  
  # Iterate through the runs in the study
  for (j in paste0("run-",1:5)){
    
    # Create a variable to capture the file name for this run
    filename <- paste0(path, "confound_export/sub-", i, "_ses-1_task-Aver_",j,"_desc-confounds_timeseries.tsv")
    
    # Check whether this file exists, and if it doesn't, print an error and give up.
    if (!file.exists(filename)){
      print(paste("Error:", i, "'s", j, "file could not be located"))
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
        df <- read.table(file = filename,
                         sep = '\t',
                         header = T,
                         na.strings = c("","NA","n/a")) %>%
      
              # subset the columns
              subset(select = confounds)
        
        # Determining whether there are more than 9 TRs that pass our threshold and creating a list of names of a length equivalent to the number of FWDs present
        if (length(which(df$framewise_displacement > threshold)) > 9){
           motion_outliers <- c(paste0("motion_outlier0", 1:9), paste0("motion_outlier", 10:length(which(df$framewise_displacement > threshold)))) 
        }
        
        # Determining whether there are less than 9 TRs that pass our threshold and creating a list of names of a length equivalent to the number of FWDs present
        if (length(which(df$framewise_displacement > threshold)) <= 9 & length(which(df$framewise_displacement > threshold)) > 0){
          motion_outliers <- c(paste0("motion_outlier0", 1:length(which(df$framewise_displacement > threshold))))          
        }
        
        # Creating an empty value if none of the FWD rise above our threshold
        if (length(which(df$framewise_displacement > threshold)) == 0){
          motion_outliers <- NULL          
        }
        
        # Creating a new dataframe to house all of the data
        if (!is.null(motion_outliers)){
          cols <- c(names(df), motion_outliers)          
        }
        if (is.null(motion_outliers)){
          cols <- names(df)          
        }
        rows <- 1:length(rownames(df))
        df_temp <- make_df(rows = rows,
                           cols = cols)
        
        # Copying data forward
        df_temp[,1:length(confounds)] <- df[,1:length(confounds)]
        if (ncol(df) != ncol(df_temp)){
          df_temp[,(length(confounds) + 1):ncol(df_temp)] <- 0
        }
        
        # Creating regressors for FWD
        if (ncol(df) != ncol(df_temp)){
          for (k in 1:nrow(df_temp)){
            if (k == 1){
              col <- (length(confounds) + 1)
            }
            if (!is.na(df_temp$framewise_displacement[k]) & df_temp$framewise_displacement[k] > threshold){
              df_temp[k,col] <- 1
              col <- col + 1
            }
          }
        }
        
        # And then save that dataframe as a new text file within that "look_onsets" folder
        write.table(df_temp, 
                    file = paste0(path,"confounds_reduced/sub-", i, "_ses-1_task-Aver_",j,"_desc-confounds_reduced.txt"), 
                    sep = "\t",
                    row.names = FALSE,
                    col.names = FALSE)
      }
    } 
  }
}