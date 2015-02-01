### rankall.R
### takes two arguments: an outcome name (outcome) and a hospital rank- ing (num). 
### The function reads the outcome-of-care-measures.csv file and returns a 2-column data 
### frame containing the hospital in each state that has the ranking specified in num. 
### Usage: rankall(outcome, num)

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

rankall <- function(outcomeName, num = "best") {

## Read outcome data
	outcomeDF <<- read.csv("./data/outcome-of-care-measures.csv", 
					colClasses = "character")

## Upper/lower case arguments and ...
## Upper case the State column in the data frame to ensure the comparisons work
	outcomeName <- tolower(outcomeName)
	outcomeDF$State <- toupper(outcomeDF$State)
	
## Validate the arguments
	validate_outcome(outcomeName)
	validate_num(num,state)


## get sorted list of states 
    statesV <- sort(unique(outcomeDF$State))

### rename the out come column to something useful
    colnames(outcomeDF)[columnIdx] <- "OutCome"
## cast the outcome column to numeric and suppress the warnings about NAs
    outcomeDF$OutCome <- suppressWarnings(as.numeric(outcomeDF$OutCome))
## omit NAs
    outcomeDF <- na.omit(outcomeDF)

## subset and sort 
    sortedDF <- outcomeDF[,c("Hospital.Name","OutCome","State")]
    completes <- complete.cases(sortedDF)
    sortedDF <- arrange(sortedDF[completes,], State, OutCome, Hospital.Name)
    dfRows <- length(statesV)

## set up empty data frame
    returnDF <<- data.frame(hospital=character(dfRows), state=character(dfRows), 
                            stringsAsFactors = FALSE)

 ## rank records by state and outcome   
    rankDF <<- ddply(sortedDF, .(State), transform, 
                         Ranking= rank(OutCome, ties.method="first"))

    if (num == "best") {
        num = 1
    } else {
        if (num == "worst") {
            worstDF <- ddply(rankDF, "State", subset
                             , Ranking==max(Ranking))
        } 
    } 
    
for(i in 1:length(statesV)) {
    state = statesV[i]
    returnDF$state[i] = state

## Get the desired hospitals
	if (num != "worst") {
		x <- rankDF[(rankDF$State==state &
                    rankDF$Ranking==num),
                    "Hospital.Name"]
 	} else {
		    x <- worstDF[worstDF$State==state
                        ,"Hospital.Name"]
 	} 
    if (length(x) == 0) {
        x = "<NA>"
    }
    returnDF$hospital[i] = x

} ## end for loop	

    returnDF
}

##TEST RESULTS: 
##> head(rankall("heart attack", 20), 10)
##                                hospital state
##1                                 <NA>    AK
##2       D W MCMILLAN MEMORIAL HOSPITAL    AL
##3    ARKANSAS METHODIST MEDICAL CENTER    AR
##4  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
##5                SHERMAN OAKS HOSPITAL    CA
##6             SKY RIDGE MEDICAL CENTER    CO
##7              MIDSTATE MEDICAL CENTER    CT
##8                                 <NA>    DC
##9                                 <NA>    DE
##10      SOUTH FLORIDA BAPTIST HOSPITAL    FL

##tail(rankall("pneumonia","worst"), 3)
##hospital state
##52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
##53                     PLATEAU MEDICAL CENTER    WV
##54           NORTH BIG HORN HOSPITAL DISTRICT    WY

##tail(rankall("heart failure"), 10)
##hospital state
##45                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
##46                                        FORT DUNCAN MEDICAL CENTER    TX
##47 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
##48                                          SENTARA POTOMAC HOSPITAL    VA
##49                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
##50                                              SPRINGFIELD HOSPITAL    VT
##51                                         HARBORVIEW MEDICAL CENTER    WA
##52                                    AURORA ST LUKES MEDICAL CENTER    WI
##53                                         FAIRMONT GENERAL HOSPITAL    WV
##54                                        CHEYENNE VA MEDICAL CENTER    WY

