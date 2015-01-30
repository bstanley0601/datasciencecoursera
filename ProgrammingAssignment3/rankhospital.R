### comments about function

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

validate_num <- function(num, state) {
    ## Check that num is "best", "worst", or a number
        if (is.na(num) || (!(num == "best" || num == "worst" || is.numeric(num)))) {
            stop("invalid num")
        }
}

###MAIN

rankhospital <- function(state, outcomeName, num) {

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
	validate_num(num,state)

    if (is.numeric(num) && (num > nrow(outcomeDF[outcomeDF$State==state,]) || num == 0)) {
		num <- NA
		return(num)		
    } else {
		num <= paste(num,"L", sep="")
	}

## Subset the data by state and outcome
    outcomeSubset <- outcomeDF[outcomeDF$State==state,c(2,columnIdx,7)]

## Rename the outcome column to something useful
    colnames(outcomeSubset)[2] <- "OutCome"

## cast the outcome column to numeric and suppress the warnings about NAs
    outcomeSubset$OutCome <- suppressWarnings(as.numeric(outcomeSubset$OutCome))

## omit NAs
    outcomeSubset <- na.omit(outcomeSubset)

## add column for rank of hospital by outcome
##	outcomeRank <- as.data.table(outcomeSubset)
##	outcomeRank[,Hospital.Rank:=rank(OutCome,ties.method="first"),]
	
## Return the desired record
	if (num == "best") {
		pick <- head(arrange(outcomeSubset, OutCome, Hospital.Name), 1L)
	} else {
		if (num == "worst") {
			pick <- tail(arrange(outcomeSubset, OutCome, Hospital.Name), 1L)
		} else {
				pick <- tail(head(arrange(outcomeSubset, 
							OutCome, Hospital.Name), num), 1L)
		}
	}
	pick$Hospital.Name
}

##TEST RESULTS: 
##rankhospital("TX", "heart failure", 4)
##[1] "DETAR HOSPITAL NAVARRO"
##> rankhospital("MD", "heart attack", "worst")
##[1] "HARFORD MEMORIAL HOSPITAL"
##> rankhospital("MN", "heart attack", 5000)
##[1] NA
