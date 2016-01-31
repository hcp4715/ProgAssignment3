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
        
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death
        
        ## rate
}