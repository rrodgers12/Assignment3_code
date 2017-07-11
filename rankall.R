# rankall.R
# Return name of hospital for each state with the specified rate

rankall <- function(outcome, num = "best") {
        # Change cases of outcome and num to all lower case to ensure conditionals work
        outcome <- tolower(outcome)
        num <- tolower(as.character(num)) # as.character in case num is passed in as an integer
        
        # Read in the data from cwd and extract only the needed columns
        # colClasses = "character" keeps R from "guessing," makes converting to numeric easier
        outcomeFullDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeResizedDF <- outcomeFullDF[ , c(2,7,11,17,23)]
        
        # Check outcome validity, stop if invalid
        # Else, set outcomeColumn based on the value of the outcome argument
        # Default value to the switch runs the stop() function
        outcomeColumn = 0
        
        switch(outcome, 
               "heart attack" = outcomeColumn <- 3,
               "heart failure" = outcomeColumn <- 4,
               "pneumonia" = outcomeColumn <- 5,
               stop("invalid outcome"))
        
        # Split the data frame up by the state column
        stateDFList <- split(outcomeResizedDF, outcomeResizedDF$State)

        # Initialize empty lists to hold states and the hospital name for those states (will be bound to DF later)
        hospital <- vector(mode = "character", length = length(stateDFList))
        state <- vector(mode = "character", length = length(stateDFList))
        
        for(i in seq_along(stateDFList)) {
                
                currentDF <- stateDFList[[i]]
                
                # Append the current state to the state vector
                state[i] <- currentDF[1, "State"]
                # Subset by removing entries with Not Available for the condition
                stateSubset <- subset(currentDF, subset = currentDF[ ,outcomeColumn] != "Not Available") 
                # Order the subset by mortality rate, then hospital name
                stateSubset <- stateSubset[order(as.numeric(stateSubset[ , outcomeColumn]), stateSubset$Hospital.Name), ]
                
                # Count the number of hospitals in the stateSubset dataframe, assign result to a variable to use if num == "worst"
                # and to check if requested rank > number of hospitals for the state
                numberOfHospitals <- nrow(stateSubset)
                rank = 0
                
                # use num argument to select correct rank
                if(num == "best") {
                        rank <- 1
                } else if (num == "worst") {
                        rank <- numberOfHospitals
                } else {
                        rank <- as.integer(num)
                }
                
                # Check if the requested rank can be returned, else take hospital name
                if(rank > numberOfHospitals) {
                        hospital[i] <- NA 
                } else {
                        hospital[i] <- stateSubset[rank, "Hospital.Name"]
                }
        }
        
        # Bind the columns and return
        finalDF <- data.frame(cbind(hospital, state))
        return(finalDF)
}



