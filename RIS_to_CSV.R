## RIS_to_CSV.R || 2020.03.21

# I need to perform a systematic review of literature. EBSCOhost will export a convenient .RIS file which I can load into Zotero
# and get all of the abstracts and citations I need, but I can't easily compile those details into a convenient reviewable format.
# This function takes an long-form .RIS file as an input and outputs a .CSV file with the desired fields

RIS_to_CSV <- function(file)
                       # title = TRUE,
                       # abstract = TRUE,
                       # keywords = TRUE,
                       # year = TRUE,
                       # primary_author = TRUE,
                       # journal = TRUE,
                       # doi = TRUE)
{
library(dplyr)
library(stringr)
source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = T)
source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/remove_NAs.R", local = T)

df <- read.csv(file,
               header = FALSE,
               na.strings = c("", "NA", " "))

# if (title = TRUE & abstract = TRUE & keywords = TRUE & year = TRUE & primary_author = TRUE & journal = TRUE & doi = TRUE){
  cols <- c("Title", "Abstract", "Keywords", "Year", "Primary_Author", "Journal", "DOI")
# }

df_new <- make_df(cols = cols,
                  rows = 1)
tracker <- 1
for (i in 1:length(rownames(df))){
  
  # Year
  if (str_detect(string = df$V1[i],
                 pattern = "Y1  - ")){
    df_new$Year[tracker] <- str_replace_all(string = df$V1[i],
                                            pattern = "Y1  - ",
                                            replace = "") %>%
      str_extract(string = ., pattern = "^....")
  }
  
  # Primary Author
  if (i > 1){
    if (str_detect(string = df$V1[i],
                   pattern = "AU  - ") &
        !str_detect(string = df$V1[i-1],
                    pattern = "AU  - ")){
      df_new$Primary_Author[tracker] <- str_replace_all(string = df$V1[i],
                                                        pattern = "AU  - ",
                                                        replace = "")
    }
  }
  
  # Title
  if (str_detect(string = df$V1[i],
                 pattern = "T1  - ")){
    df_new$Title[tracker] <- str_replace_all(string = df$V1[i],
                                             pattern = "T1  - ",
                                             replace = "")
  }
  
  # Journal
  if (str_detect(string = df$V1[i],
                 pattern = "JO  - ")){
    df_new$Journal[tracker] <- str_replace_all(string = df$V1[i],
                                               pattern = "JO  - ",
                                               replace = "")
  }
  
  # Abstract
  if (str_detect(string = df$V1[i],
                 pattern = "AB  - ")){
    df_new$Abstract[tracker] <- str_replace_all(string = df$V1[i],
                                                pattern = "AB  - ",
                                                replace = "")
  }

  # Keywords
  if (str_detect(string = df$V1[i],
                 pattern = "KW  - ")){
    if (is.na(df_new$Keywords[tracker])){
      df_new$Keywords[tracker] <- str_replace_all(string = df$V1[i],
                                                  pattern = "KW  - ",
                                                  replace = "")
    }
    if (!is.na(df_new$Keywords[tracker])){
        df_new$Keywords[tracker] <- paste(df_new$Keywords[tracker], 
                                      str_replace_all(string = df$V1[i],
                                                      pattern = "KW  - ",
                                                      replace = ""),
                                      sep = ", ")
    }
  }
  
  # DOI
  if (str_detect(string = df$V1[i],
                 pattern = "DO  - ")){
    df_new$DOI[tracker] <- str_replace_all(string = df$V1[i],
                                           pattern = "DO  - ",
                                           replace = "")
  }
  
  # Next Entry
  if (str_detect(string = df$V1[i],
                  pattern = "ER  - ")){
    df_temp <- make_df(cols = cols,
                       rows = 1)
    df_new <- rbind(df_new,df_temp)
    rm(df_temp)
    tracker <- tracker + 1
  }
}

citations <- remove_NAs(df_new)

return(citations)

}
