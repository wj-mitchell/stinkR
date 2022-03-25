library(assertthat)
library(dplyr)
library(stringr)

# Identifying where the data exists
path <- "S:/Helion_Group/studies/rsa_moral/data/deriv/"

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
                         header = T) %>%
              subset(select = c())
        # subset the columns
        
        # And then save that dataframe as a new text file within that "look_onsets" folder
        write.table(df_temp, 
                      file = paste0(path,"look_onsets/", i ,"_", j, "_trial", k,".tsv"), 
                      sep = "\t",
                      row.names = FALSE,
                      col.names = FALSE)
      }
    } 
  }
}