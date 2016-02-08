## This script is to rank the hospitals based on the mortality data for every state

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        df[,11] <- as.numeric(df[,11])
        df[,17] <- as.numeric(df[,17])
        df[,23] <- as.numeric(df[,23])
        results <- c()
        ## Check that state and outcome are valid
        if (outcome == "heart failure"){
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                state_df <- valid_df[valid_df$State == state,]
                temp <- state_df[,c(2,17)]
                state_rank <- temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,temp$Hospital.Name),]
                state_rank$Rank <- 1:nrow(state_rank)
                if (num == "best"){
                        results <- head(state_rank$Hospital.Name)[1]
                } else if (num == "worst"){
                        results <- tail(state_rank$Hospital.Name,1)
                } else if(num > nrow(state_rank)){
                        results <- NA
                } else {
                        results <- state_rank$Hospital.Name[num]
                }
        } else if (outcome == "heart attack") {
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                state_df <- valid_df[valid_df$State == state,]
                temp <- state_df[,c(2,11)]
                state_rank <- temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,temp$Hospital.Name),]
                state_rank$Rank <- 1:nrow(state_rank)
                if (num == "best"){
                        results <- head(state_rank$Hospital.Name)[1]
                } else if (num == "worst"){
                        results <- tail(state_rank$Hospital.Name,1)
                } else if(num > nrow(state_rank)){
                        results <- NA
                } else {
                        results <- state_rank$Hospital.Name[num]
                }
        } else {
                
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                state_df <- valid_df[valid_df$State == state,]
                temp <- state_df[,c(2,23)]
                state_rank <- temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,temp$Hospital.Name),]
                state_rank$Rank <- 1:nrow(state_rank)
                if (num == "best"){
                        results <- head(state_rank$Hospital.Name)[1]
                } else if (num == "worst"){
                        results <- tail(state_rank$Hospital.Name,1)
                } else if(num > nrow(state_rank)){
                        results <- NA
                } else {
                        results <- state_rank$Hospital.Name[num]
                }
        }
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        results
        
}