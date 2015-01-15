pollutantmean <- function(directory, pollutant, id = 1:332) {
  # A vector of numeric to store interesting non-NA values accross all files.
  pollutant_values <- vector(mode = "numeric")
  for(i in id) {
    # Format the current number to width 3 with leading 0 if needed
    file_number <- formatC(i, width = 3, format = "d", flag = "0")
    
    # Get the associated file name for this iteration
    file = paste(directory, "/", file_number, ".csv", sep = "")
    
    # Read in the file data
    data <- read.csv(file, header = TRUE)
    
    # Get the non-NA values in the column of the pollutant required
    vals <- data[complete.cases(data[,pollutant]), pollutant]
    
    # Append to the vector of interesting values
    pollutant_values <- append(pollutant_values, vals)
  }
  m <- mean(pollutant_values)
  # I'm only rounding here to match the example expected output
  # It wasn't a specific requirement in the question itself
  round(m, 3)
}