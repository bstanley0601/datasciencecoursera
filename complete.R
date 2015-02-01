complete <- function(directory = "~/datasciencecoursera/specdata", id = 1:332) {
    library(plyr)
    ## should check existence of directory here
    
    completes <- data.frame(id = character(), nobs = numeric())
    
    for (x in id) {
        fid <- as.integer(x)
        fname <- paste(sprintf("%03d", fid), ".csv", sep="")
        fname <- paste(directory, fname, sep="/")
        idDF <- read.csv(fname)
        completes <- rbind(completes, ddply(idDF[complete.cases(idDF), ], c("ID"), nrow))
    }
    names(completes) <- c("id","nobs")
    return(completes)
}