corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  fcorr <- function(file) {                      ## calculate the correlation on a file
    data <- read.csv(file.path(directory, file))  ## read the data
    nobs <- sum(complete.cases(data))             ## calculate # of complete cases
    ## if we have more cases than the threshold ... calculate correlation
    if (nobs > threshold) cor(data$nitrate, data$sulfate, use="complete.obs")   
  }
  lcor <- sapply(list.files(directory), fcorr)  ## apply to every file in the directory                    
  lcor <- unlist(lcor[!sapply(lcor, is.null)])  ## remove null and make vector                  
  return (lcor)
}




 




