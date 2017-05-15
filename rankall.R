rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ndata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        colName <- names(ndata)
        
        states <- levels(factor(ndata[,7]))
        hospital <- vector(mode="character")
        
        source("rankhospital.R")
        for (i in seq(states)) {
                hospital[i] = rankhospital(states[i], outcome, num)
        }
        
        df <- data.frame(hospital, states) 
        #colnames(df) <- c("state", "hospital")
}
