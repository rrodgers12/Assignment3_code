# old_best.R

best <- function(state, outcome) {
        # Change cases of state to all caps, outcome to all lower case to ensure conditionals work
        state <- toupper(state)
        outcome <- tolower(outcome)
        
        # Read in the data from cwd
        # Pull out only needed columns:
        # 2 = name, 7 = state
        # "heart attack" = col 11, "heart failure" = col 17, "pneumonia" = col 23
        outcomeFullDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeResizedDF <- outcomeFullDF[ , c(2,7,11,17,23)]
        
        # Check state validity, stop if invalid
        validStates <- unique(outcomeResizedDF$State)
        if(is.element(state, validStates) == FALSE) {
                stop("invalid state")
        }
        
        # Check outcome validity, stop if invalid
        # Else, set the column number variable to coincide with correct column in DF
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomeColumn = 0
        
        if(is.element(outcome, validOutcomes) == FALSE) {
                stop("invalid outcome") 
        } else if(outcome == "heart attack") {
                outcomeColumn = 3
        } else if(outcome == "heart failure") {
                outcomeColumn = 4
        } else if(outcome == "pneumonia") {
                outcomeColumn = 5
        }

        # Subset the full data frame by state and removing entries with missing data 
        # in specified condition column
        stateSubset <- subset(outcomeResizedDF, subset = outcomeResizedDF$State == state & outcomeResizedDF[ , outcomeColumn] != "Not Available")
        
        stateSubset <- stateSubset[order(as.numeric(stateSubset[ , outcomeColumn]), stateSubset$Hospital.Name), ]
        
        return(as.vector(stateSubset[1, "Hospital.Name"]))
        
}

