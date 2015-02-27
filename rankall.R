rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # outcome <- "heart attack" 
        # diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        #
        ## Check that state and outcome are valid
        if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        } else {
                
                ## For each state, find the hospital of the given rank
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                if (outcome == "heart attack") {
                        diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                        n <- 11
                } else if (outcome == "heart failure") {
                        diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                        n <- 17
                } else {
                        diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                        n <- 23
                }
                
                ## New data frame withot omitted data
                df1 <- data.frame()
                df1 <- df[complete.cases(suppressWarnings(as.numeric(df[,diseas]))),]
                df1 <- data.frame()
                
                #2 name, 7 state, 23 pneumonia
                df1 <- df[complete.cases(suppressWarnings(as.numeric(df[,diseas]))),c(2, 7, n)]
                df1[,3] <- as.numeric(df1[,3])
                df1[,2] <- as.factor(df1[,2])
                
                df1 <- df1[order(df1[,2],df1[,3],df1[,1]),]
                ## Choose rows that respond to our state
                ## Search for the hospital
                l <- split(df1, df1[,2])
                
                result <- data.frame("Hospital Name"=character(0), "State"=character(0), "Diseas Mortality Level"=numeric(0))
                #colnames(result) <- c("Hospital Name", "State", "Diseas Mortality Level") 
                colnames(result) <- colnames(df1)
                
                for (i in 1:length(l)) {
                        df_state <- as.data.frame(l[i][1])
                        colnames(df_state) <- colnames(df1)
                        if (num == "best") {
                                result <- rbind(result, df_state[1,])                                
                        } else if (num == "worst"){
                                result <- rbind(result, df_state[length(df_state[,3]),])          
                        } else {
                                result <- rbind(result, df_state[num,])
                        }
                }
                colnames(result) <- c("hospital", "state", "Diseas Mortality Level")
                result[,1:2]           
        }            
}

head(rankall("heart attack", 20), 10)
