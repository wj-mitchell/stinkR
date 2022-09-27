# progress_report || 2022.09.22

# IN DEVELOPMENT

# When placed in a for loop, progress report will output in the console 
# the percentage of the for loop that has completed, the time at which
# it has completed, and additionally, an estimation of how much time is 
# left, the calculations for which dynamically update as more iterations
# of the loop are ran. All you need to enter is what your for loop variable
# is and the array that the for loop is iterating through. 

progress_report <- function(i, # i represents the term you are using in your for loop; the left side of "for (__ in array)". 
                               # If you're using a nested for loop, I'd recommend entering the term for the highest-level loop.
                            array) # the array that your for loop is iterating through; the right side of "for (i in ______)"
{
  progress <- which(array == i)
  start_time <- Sys.time()
  end_time <- Sys.time()
  if (progress == 1){
    mean_run_time <- as.numeric(end_time - start_time)
  }
  if (progress != 1){
    mean_run_time <- mean(rep(mean_run_time, progress), as.numeric(end_time - start_time))
  }
  
  percent <- round(progress/length(array) * 100, 1)
  remaining_time <- round((mean_run_time * (length(array) - progress)) / 60, 1)
  paste0(percent, "% Completed at ", Sys.time(), " || Estimated Time Remaining: ", remaining_time, " minutes") %>%
    print()
}
