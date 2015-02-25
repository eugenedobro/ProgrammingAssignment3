## Function returns the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state.
## 
## Input: 
##      character vector state -- abbreviated name of a state;
##      character vector outcome -- an outcome name;
## Output:
##      character vector with the name of the 'Best' hospital in state
##
best <- funbest <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!(state %in% df[,7])) {
                stop("invalid state")
        } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        } else {
                
                ## Return hospital name in that state with lowest 30-day death
                ## rate
                diseas <- if (outcome == "heart attack") {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                } else if (outcome == "heart failure") {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                } else {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                }
                
                ## New data frame withot omitted data
                df1 <- data.frame()
                df1 <- df[complete.cases(suppressWarnings(as.numeric(df[,diseas]))),]
                states <- as.factor(df1[, 7])
                
                ## Choose rows that respond to our state
                df1 <- df1[which(as.factor(df1[, 7]) == state), ]
                ## Search for all minimum values in outcome
                df1 <- df1[which(as.numeric(df1[,diseas]) == min(as.numeric(df1[,diseas]))), ]
                
                ##return the name of the best hospital
                as.character(df1[,2])
                
        }     
        
}