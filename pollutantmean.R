pollutantmean <- function(directory = getwd(), pollutant = "nitrate", id = 1:332) {
    ## should check existence of directory here
    
    fnames <- sprintf("%03d.csv", id)
    files <- paste(directory, fnames, sep="/")
    ## should check existence of files here
    
    ## read and merge the files into a single data frame
    id_ds <- do.call(rbind,lapply(files, read.csv))
    
    ##calculate the mean of the pollutant in question, removing NA values
    pMean <- round(mean(id_ds[,pollutant], na.rm=TRUE), digits = 3)
    
    return(pMean)
}