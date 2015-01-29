best <- function(state = "CO", 
                 outcomeName = "heart attack") {
    ## Read outcome data; suppress NAs
    outcomeDF <<- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
    outcomeDF <<- na.omit(outcomeDF)
    ## upper case the state argument; upper case the state column; 
    ## Check that state is present in the data
    state <- toupper(state)
    outcomeDF$State <- toupper(outcomeDF$State)
    if (!state %in% outcomeDF$State) {
##        message("ERROR: state not found in outcome-of-care-measures.csv")
        stop("invalid state")
    }
    ## Check that outcomeName is valid and set the column index 
    if (outcomeName == "heart attack") {
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
    
    return(bestHospital)
}