## Function returns the name of the hospital that has the NUM rank 
## in 30-day mortality for the specified outcome in specific state.
## 
## Input: 
##      character vector state -- abbreviated name of a state;
##      character vector outcome -- an outcome name;
##      character or numerica num -- take values “best”, “worst”, or an integer indicating the ranking
## Output:
##      character vector with the name of the num-ranked hospital in the state
##

rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!(state %in% df[,7])) {
                stop("invalid state")
        } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        } else {
                
                ## Return hospital name in that state with the given rank
                ## 30-day death rate
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
                ## Search for the hospital
                if (num == "best"){
                        df1 <- df1[which(as.numeric(df1[,diseas]) == min(as.numeric(df1[,diseas]))), ]
                        res <- as.character(df1[,2])
                } else if (num == "worst"){
                        df1 <- df1[which(as.numeric(df1[,diseas]) == max(as.numeric(df1[,diseas]))), ]
                        res <- as.character(df1[,2])
                } else {
                        df2 <- data.frame()
                        df2 <- df1[order(as.numeric(df1[,diseas]), df1[,2]),] 
                        res <- df2[num, 2]
                }
                
                ##return the name of the numth best hospital
                res        
        }            
        
}

##testing
#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)
#rankhospital("TX", "heart failure", "best")
