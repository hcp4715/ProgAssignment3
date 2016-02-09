## This script is to rank the hospitals based on the mortality data for every state

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        df[,11] <- as.numeric(df[,11])
        df[,17] <- as.numeric(df[,17])
        df[,23] <- as.numeric(df[,23])
        results <- c()
        states <- unique(df$State)
        ## Check that state and outcome are valid
        if (outcome == "heart failure"){
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                temp <- valid_df[,c(2,7,17)]
                for (i in 1:length(states)) {
                        results$state[i]<- states[i]
                        state_df <- temp[temp$State == states[i],]
                        state_rank <- state_df[order(state_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,state_df$Hospital.Name),]
                        
                        if (num == "best"){
                                Rank <- head(state_rank$Hospital.Name)[1]
                        } else if (num == "worst"){
                                Rank <- tail(state_rank$Hospital.Name,1)
                        } else if(num > nrow(state_rank)){
                                Rank <- NA
                        } else {
                                Rank <- state_rank$Hospital.Name[num]
                        }
                        results$hospital[i] <- Rank
                        
                }
                
        } else if (outcome == "heart attack") {
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                temp <- valid_df[,c(2,7,11)]
                for (i in 1:length(states)) {
                        results$state[i]<- states[i]
                        state_df <- temp[temp$State == states[i],]
                        state_rank <- state_df[order(state_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,state_df$Hospital.Name),]
                        
                        if (num == "best"){
                                Rank <- head(state_rank$Hospital.Name)[1]
                        } else if (num == "worst"){
                                Rank <- tail(state_rank$Hospital.Name,1)
                        } else if(num > nrow(state_rank)){
                                Rank <- NA
                        } else {
                                Rank <- state_rank$Hospital.Name[num]
                        }
                        results$hospital[i] <- Rank
                        
                }
                
        } else {
                
                valid_df <- df[complete.cases(df$State) & complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                temp <- valid_df[,c(2,7,23)]
                for (i in 1:length(states)) {
                        results$state[i]<- states[i]
                        state_df <- temp[temp$State == states[i],]
                        state_rank <- state_df[order(state_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,state_df$Hospital.Name),]
                        
                        if (num == "best"){
                                Rank <- head(state_rank$Hospital.Name)[1]
                        } else if (num == "worst"){
                                Rank <- tail(state_rank$Hospital.Name,1)
                        } else if(num > nrow(state_rank)){
                                Rank <- NA
                        } else {
                                Rank <- state_rank$Hospital.Name[num]
                        }
                        results$hospital[i] <- Rank
                        
                }
        }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        results <- data.frame(results)
        results <- results[,c(2,1)]
        results <- results[order(results$state),]
}