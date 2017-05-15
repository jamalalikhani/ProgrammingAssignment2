rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ndata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        colName <- names(ndata)
        
        ## Check that state and outcome are valid
        states <- levels(factor(ndata[,7]))
        if(sum(states==state) == 0) stop("invalid state")
        valid_outcome = c("heart attack", "heart failure", "pneumonia")
        if(sum(valid_outcome==outcome) == 0) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death rate
        data <- subset(ndata, State == state)
        if (outcome == valid_outcome[1])  coln <- colName[11]
        if (outcome == valid_outcome[2])  coln <- colName[17]
        if (outcome == valid_outcome[3])  coln <- colName[23]
        
        data[, coln] <- as.numeric(data[, coln])
        data <- data[!is.na(data[, coln]), ]
        data <- data[order(data[, coln], data[, 2]), ]             
        
        totalHos <- length(data[, coln])
        
        if (num == "best") {
                return (data[1, 2])
        }
        else if (num == "worst"){
                return (data[totalHos, 2])
        }
        else if (num > totalHos){
                return (NA)
        }
        else {
                return ((data[num, 2]))
        }
}
