best <- function(state, outcome) {
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
                
                
                df1 <- data.frame()
                df1 <- df[complete.cases(as.numeric(df[,diseas])),]
                states <- as.factor(df1[, 7])
                
                df1 <- df1[which(as.factor(df1[, 7]) == state), ]
                df1 <- df1[which(as.numeric(df1[,diseas]) == min(as.numeric(df1[,diseas]))), ]
                
                as.character(df1[,2])
                
        }     
        
}