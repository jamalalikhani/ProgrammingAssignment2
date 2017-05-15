best <- function(state, outcome) {
        ## Read outcome data
        ndata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        colName <- names(ndata)
        
        ## Check that state and outcome are valid
        if(sum(state.abb==state) == 0) stop("invalid state")
        valid_outcome = c("heart attack", "heart failure", "pneumonia")
        if(sum(valid_outcome==outcome) == 0) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death rate
        data <- subset(ndata, State == state)
        if (outcome == valid_outcome[1])  coln <- colName[11]
        if (outcome == valid_outcome[2])  coln <- colName[17]
        if (outcome == valid_outcome[3])  coln <- colName[23]
        
        data[, coln] <- as.numeric(data[, coln])
        
        index_val <- min(data[, coln], na.rm = TRUE)
        output1 <- data[data[, coln]==index_val, 2]
        output2 <- sort(output1)
        return(output2[1])
}