library(assertthat)
library(dplyr)
library(stringr)

# Identifying where the data exists
path <- "S:/Helion_Group/studies/rsa_moral/data/source/"

# Creating a single master directory to host all of the new txt files
dir.create(paste0(path,"close_onsets"))

# Identifying the different participant IDs
pts <- list.files(path = path) %>%
       str_extract(pattern = "^SC0....|^CU1....") %>% 
       subset(!is.na(.)) %>%
       unique()

# Iterating through each of the participants
for (i in pts){
  
  # Check whether a participant directory exists for each of them
  if (!dir.exists(paste0(path,i))){
    print(paste("Participant", i,'s directory could not be located'))
  }  
  
  # If a participant directory exists . . .
  if (dir.exists(paste0(path,i))){
    
    # Create a new variable for their filepaths . . . 
    dirname <- paste0(path,i,"/p1/behav")
    
    # And check if the filepath is valid . . . 
    if (!dir.exists(dirname)){
      
      # If it's not, try this other filepath
      dirname <- paste0(path,i,"/p2/behav")
      
      # If it's also not valid, give up on this one and go on to the next after printing an error message
      if (!dir.exists(dirname)){
        print(paste("Error:", i,"'s data could not be located"))
      }
    }
    
    # If the filepath is valid . . . 
    if (dir.exists(dirname)){
      
      # Iterate through the runs in the study
      for (j in paste0("run",1:5)){
        
        # Create a new variable for the run filepath 
        subdirname <- paste0(dirname,"/", j)
        
        # If the filepath is not valid , , , 
        if (!dir.exists(subdirname)){
          
          # Try this other file path 
          subdirname <- paste0(dirname,"/n_", j)
          
          # If it's also not valid, give up and print this error
          if (!dir.exists(subdirname)){
            print(paste("Error:", i,"'s", j, "directory could not be located"))
          }
        }
        
        # If the filepath does exist
        if (dir.exists(subdirname)){
          
          # Create a variable to capture the file name for this run
          filename <- paste0(subdirname,"/close_", j,".txt")
          
          # Check whether this file exists, and if it doesn't, print an error and give up.
          if (!file.exists(filename)){
            print(paste("Error:", i,"'s ", j,"file could not be located"))
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
                               header = F)
              
              # Then iterate through each of its rows
              for (k in 1:length(rownames(df))){
                
                # Create a new dataframe for each row . . . 
                df_temp <- df[k,]
                
                # And then save that dataframe as a new text file within that "look_onsets" folder
                write.table(df_temp, 
                            file = paste0(path,"close_onsets/", i ,"_", j, "_trial", k,".txt"), 
                            sep = "\t",
                            row.names = FALSE,
                            col.names = FALSE)
              }
            }
          } 
        }
      }
    } 
  }
}      
