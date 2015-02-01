corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all variables)
    ## required to compute the correlation between nitrate and 
    ## sulfate; the default is 0 
    
    ## Return a numeric vector of correlations
    library(plyr)
    source("complete.R")
    
    files <- list.files(path=directory, pattern=".csv", full.names=TRUE)
    fileDF <- do.call(rbind,lapply(files, read.csv))
    
    completes <- complete(directory)
    tIDs <- completes$id[completes$nobs > threshold]
    cors <- rep(0, length(tIDs))
    i = 1
    x = 0
    for (x in tIDs) {
        #print(x)
        corDF <- fileDF[complete.cases(fileDF) & fileDF$ID == x, ]
        #print(corDF)
        cors[i] <- cor(corDF$nitrate, corDF$sulfate)
        i <- i + 1
    }
    return(cors)

}