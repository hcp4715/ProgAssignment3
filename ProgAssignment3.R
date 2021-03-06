## R script for assigment 3

## part 1, plot 30-day mortality rate for heart attach

outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])


## part 2, finding the best hopital in a state

best <- function(state,outcome){
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        df[,11] <- as.numeric(df[,11])
        df[,17] <- as.numeric(df[,17])
        df[,23] <- as.numeric(df[,23])
        results <- c()
        
        ## for heart attack
        
        ## Check that state and outcome are valid
        if (outcome == "heart failure"){
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                valid_df <- data.table(valide_df)
                bestHP <- valid_df[,.SD[which.min(`Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure`)],by=State]
                results <- bestHP$`Hospital.Name`[bestHP$State == state]
        } else if (outcome == "heart attack") {
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                valid_df <- data.table(valide_df)
                bestHP <- valid_df[,.SD[which.min(`Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack`)],by=State]
                results <- bestHP$`Hospital.Name`[bestHP$State == state]
        } else {
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                valid_df <- data.table(valide_df)
                bestHP <- valid_df[,.SD[which.min(`Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia`)],by=State]
                results <- bestHP$`Hospital.Name`[bestHP$State == state]
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        results
}