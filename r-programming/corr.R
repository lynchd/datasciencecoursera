corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  correlations <- vector(mode = "numeric")
  for(file in list.files(directory, full.names = TRUE)) {
    data <- read.csv(file, header = TRUE)
    count <- sum(complete.cases(data))
    # skip if there are too few points
    if (count <= threshold) { next; }
    nitrate <- data[, "nitrate"]
    sulfate <- data[, "sulfate"]
    correlation <- cor(nitrate, sulfate, use ="complete.obs")
    correlations <- append(correlations, correlation, 5)
  }
  correlations
}