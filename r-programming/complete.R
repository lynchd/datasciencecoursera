complete <- function(directory, id = 1:332) {
  df <- data.frame(id=numeric(0), nobs=integer(0))
  for(i in id) {
    # Format the current number to width 3 with leading 0 if needed
    file_number <- formatC(i, width = 3, format = "d", flag = "0")
    
    # Get the associated file name for this iteration
    file = paste(directory, "/", file_number, ".csv", sep = "")
    
    # Read in the file data
    data <- read.csv(file, header = TRUE)
    
    # Count the complete cases in the sample
    count <- sum(complete.cases(data))
    
    # Bind the results to a data frame row
    df <- rbind(df, c(i, count))
  }
  # Setting the names 
  names(df)[1] = "id"
  names(df)[2] = "nobs"
  df
}