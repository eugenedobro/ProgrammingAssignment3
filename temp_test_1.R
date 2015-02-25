best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (!(state %in% df[,7])) {
                "invalid state"
                stop("invalid state", call.=FALSE)
        }
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                "invalid outcome"
                stop
        }
        
        2 + 1
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
}

best("AL", outcome = "pneumonia")
best("tx", "stuff")
df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
str(df)
df[,1]
names(df)
ha <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
hf <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
pn <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
outc <- ha

df1 <- data.frame()
df1 <- df[complete.cases(as.numeric(df[,11])),]
tail(df1)

x <- as.numeric(df1[,outc])
x

z <- as.factor(df1[, 7])
z
tapply(x, z == "TX", min)


df1 <- df[,((df[,7])=="TX")]

min(c(10,20,30))
min(c(10,10, 20, 30))
min(c(16.5, 15.1, 15.3, 13.7, 15.4, 14.6, 16.4, 17.8, 14.2, 14.8, 15.4, 14.9))

spl <- split(x, z == "TX")
spl[2]
names(spl)
lapply(spl[2], min)


as.numeric(tapply(x, z == "TX", min)[2])



df2 <- data.frame()
df2 <- df[complete.cases(as.numeric(df[,ha])),]

