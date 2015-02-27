rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # outcome <- "heart attack" 
        # diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        #
        ## Check that state and outcome are valid
        if (!(state %in% df[,7])) {
                stop("invalid state")
        } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        } else {
                
                ## For each state, find the hospital of the given rank
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
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
                
                ## Choose rows that respond to our state
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


##########################
df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome <- "pneumonia" 
diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
result <- data.frame()
df1 <- data.frame()

#2 name, 7 state, 23 pneumonia
df1 <- df[complete.cases(suppressWarnings(as.numeric(df[,diseas]))),c(2, 7, 23)]
df1[,3] <- as.numeric(df1[,3])
df1[,2] <- as.factor(df1[,2])
#df1 <- df1[order(df1[,1]),]
#df1[,1]
#head(df1)



func <- function (num = "best") {
        if (num == "best"){
                df1 <- df1[which(df1[,diseas] == min(df1[,diseas])), ]
                res <- as.character(df1[,2])
        } else if (num == "worst"){
                df1 <- df1[which(as.numeric(df1[,diseas]) == max(as.numeric(df1[,diseas]))), ]
                res <- as.character(df1[,2])
        } else {
                df2 <- data.frame()
                df2 <- df1[order(as.numeric(df1[,diseas]), df1[,2]),] 
                res <- as.character(df2[num, 2])
        }
}



##########################
df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome <- "pneumonia" 
diseas <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
num = "best"

result <- data.frame()
df1 <- data.frame()

#2 name, 7 state, 23 pneumonia
df1 <- df[complete.cases(suppressWarnings(as.numeric(df[,diseas]))),c(2, 7, 23)]
df1[,3] <- as.numeric(df1[,3])
df1[,2] <- as.factor(df1[,2])

df1 <- df1[order(df1[,2],df1[,3],df1[,1]),]
#head(df1[,3])
str(df1)
names(df1)[3]<-"Pneumonia"
head(df1, 15)

l <- split(df1, df1[,2])
str(l)
class(l[1][1])
l[1][1]
l[length(l)][1]
result <- as.data.frame(l[1][1])
result

for (i in 1:lenght(l)) {
        
}

res <- data.frame()
#res <- do.call(rbind, by(df1, df1[,2], head, 1))
#res
rankCompany <- function(num) {
        result <- data.frame()
        if (num == "best") {
                result <- do.call(rbind, by(df1, df1[,2], head, 1))
        } else if (num == "worst") {
                result <- do.call(rbind, by(df1, df1[,2], tail, 1))
        } else {
                result <- merge(result, data.frame(Category = df1[,2], Ranking = num), all.y = TRUE)
        }
        result
}

rankCompany(num = "worst")
rankCompany(num = "best")


