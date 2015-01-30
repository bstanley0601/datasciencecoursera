library(data.table); library(plyr); library(dplyr)
best <- function(state = NA, 
                 outcomeName = NA) {
    ## Read outcome data; suppress NAs
    outcomeDF <<- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")

    ## upper case the state argument; upper case the state column; 
    ## Check that state is present in the data
    state <- toupper(state)
    outcomeDF$State <- toupper(outcomeDF$State)
    if (is.na(state) || !state %in% outcomeDF$State) {
##        message("ERROR: state not found in outcome-of-care-measures.csv")
        stop("invalid state")
    }
    ## Check that outcomeName is valid and set the column index 
    if (is.na(outcomeName)) {
        stop("invalid outcome")
    }
    outcomeName <- tolower(outcomeName)
    if (outcomeName == "heart attack" ) {
        columnIdx = 11
    } else {
        if (outcomeName == "heart failure") {
            columnIdx = 17
        } else {
            if (outcomeName == "pneumonia") {
                columnIdx = 23
            } else {
##                message("ERROR: Invalid outcome name.")
##                message("Valid values are 'heart attack', 'heart failure', 'pneumonia'.")
                stop("invalid outcome")
            }
        }
    }
    ## Return hospital name in that state with lowest 30-day death rate
    
    ## Subset the data by state and outcome
    outcomeSubset <- outcomeDF[outcomeDF$State==state,c(2,columnIdx,7)]
    ## rename the outcome column to something useful
    colnames(outcomeSubset)[2] <- "OutCome"
    ## remove the larger data frame
    rm(outcomeDF)
    ## cast the outcome column to numeric and suppress the warnings about NAs
    outcomeSubset$OutCome <- suppressWarnings(as.numeric(outcomeSubset$OutCome))
    ##omit NAs
    outcomeSubset <- na.omit(outcomeSubset)
    outcomeDT <- data.table(outcomeSubset, key="OutCome")
    outcomeBest <- head(outcomeDT[,list(best=min(OutCome)), by=Hospital.Name], 1L)
    
    outcomeBest$Hospital.Name
    
}

##> best("TX","heart attack")
##[1] "CYPRESS FAIRBANKS MEDICAL CENTER"

##> best("TX","heart failure")
##[1] "FORT DUNCAN MEDICAL CENTER"

##> best("MD", "heart attack")
##[1] "JOHNS HOPKINS HOSPITAL, THE"

##> best("MD", "pneumonia")
##[1] "GREATER BALTIMORE MEDICAL CENTER"

##> best("BB", "heart attack")
##Error in best("BB", "heart attack") : invalid state

##> best("NY", "hert attack")
##Error in best("NY", "hert attack") : invalid outcome