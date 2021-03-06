## The best() function reads the outcome-of-care-measures.csv
## file and returns a character vector with the name of the 
## hospital that has the best (i.e. lowest) 30-day mortality 
## for the specied outcome in the specified state. 

library(data.table); library(plyr); library(dplyr)
 
validate_state <- function(state) {
    ## Check that state is present in the data    
    if (is.na(state) || !state %in% outcomeDF$State) {
##        message("ERROR: state not found in outcome-of-care-measures.csv")
        stop("invalid state")
    }
}

validate_outcome <- function(outcomeName) {
    ## Check that outcomeName is valid and set the column index 
    if (is.na(outcomeName)) {
        stop("invalid outcome")
    }
    outcomeName <- tolower(outcomeName)
    if (outcomeName == "heart attack" ) {
        columnIdx <<- 11
    } else {
        if (outcomeName == "heart failure") {
            columnIdx <<- 17
        } else {
            if (outcomeName == "pneumonia") {
                columnIdx <<- 23
            } else {
##                message("ERROR: Invalid outcome name.")
##                message("Valid values are 'heart attack', 'heart failure', 'pneumonia'.")
                stop("invalid outcome")
            }
        }
    }
}

###MAIN

best <- function(state = NA, 
                 outcomeName = NA) {

## Read outcome data
	outcomeDF <<- read.csv("./data/outcome-of-care-measures.csv", 
					colClasses = "character")

## Upper/lower case arguments and ...
## Upper case the State column in the data frame to ensure the comparisons work
	state <- toupper(state)
	outcomeName <- tolower(outcomeName)
	outcomeDF$State <- toupper(outcomeDF$State)
	
## Validate the arguments
	validate_state(state)
	validate_outcome(outcomeName)
	    
        
## Subset the data by state and outcome
    outcomeSubset <- outcomeDF[outcomeDF$State==state,c(2,columnIdx,7)]
    ## remove the larger data frame
    ## rm(outcomeDF)

## Rename the outcome column to something useful
    colnames(outcomeSubset)[2] <- "OutCome"

## cast the outcome column to numeric and suppress the warnings about NAs
    outcomeSubset$OutCome <- suppressWarnings(as.numeric(outcomeSubset$OutCome))
    
##omit NAs
    outcomeSubset <- na.omit(outcomeSubset)

## get best using arrange then head the first record
    outcomeSort <- arrange(outcomeSubset, OutCome, Hospital.Name)
    outcomeBest <- head(outcomeSort, 1L)

    
## Return hospital name in that state with lowest 30-day death rate
	outcomeBest$Hospital.Name
    
}

##TEST RESULTS: 
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