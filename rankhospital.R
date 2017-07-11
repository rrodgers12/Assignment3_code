# rankhospital
# Return name of hospital with specified rank for specified condition

rankhospital <- function(state, outcome, num = "best") {
        # Change cases of state to all caps, outcome and num to all lower case to ensure conditionals work
        state <- toupper(state)
        outcome <- tolower(outcome)
        num <- tolower(as.character(num)) # as.character in case num is passed in as an integer
        
        # Read in the data from cwd and extract only the needed columns
        # colClasses = "character" keeps R from "guessing," makes converting to numeric easier
        outcomeFullDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeResizedDF <- outcomeFullDF[ , c(2,7,11,17,23)]
        
        # Check state validity, stop if invalid
        validStates <- unique(outcomeResizedDF$State)
        if(is.element(state, validStates) == FALSE) {
                stop("invalid state")
        }
        
        # Check outcome validity, stop if invalid
        # Else, set outcomeColumn based on the value of the outcome argument
        # Default value to the switch runs the stop() function
        outcomeColumn = 0
        
        switch(outcome, 
               "heart attack" = outcomeColumn <- 3,
               "heart failure" = outcomeColumn <- 4,
               "pneumonia" = outcomeColumn <- 5,
               stop("invalid outcome"))
        
        # Subset the full data frame by state and removing entries with missing data in specied condition column 
        stateSubset <- subset(outcomeResizedDF, subset = outcomeResizedDF$State == state & outcomeResizedDF[ , outcomeColumn] != "Not Available")
        # Order by mortality rate, then hospital name
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
        
        # Check if the requested rank can be returned
        if(rank > numberOfHospitals) {
                return(NA) 
        } else {
                return(stateSubset[rank, "Hospital.Name"])
                }
        
}

        
        
        