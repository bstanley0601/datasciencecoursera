best <- function(state = "CO", 
                 outcomeName = "heart attack") {
    ## Read outcome data
    outcomeDF <<- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state is present in the data
    if (!state %in% outcomeDF$State) {
        message("ERROR: state not found in outcome-of-care-measures.csv")
        return(1)
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
                message("ERROR: Invalid outcome name.")
                message("Valid values are 'heart attack', 'heart failure', 'pneumonia'.")
                return(2)
            }
        }
    }
    ## Return hospital name in that state with lowest 30-day death rate
    
}