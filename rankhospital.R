rankhospital <- function(state, outcome, num = "best") {
  
  # used for test purposes
  # state <- "TX"
  # outcome <- "heart failure"
  # num = 5
  
  
  # Map all valid outcomes to relevant mortality rate colname in data file
  validOutcomes = list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  # Load data 
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # verify if requested state exists
  if(!(state %in% data$State)) {
    
    stop('invalid state')
  }
  
  # verify if requested outcome exists
  if(!(length(outcome) == 1 && outcome %in% names(validOutcomes))){
    
    stop('invalid outcome')
  }
  
  statehospitals <- data[ (data$State == state) == TRUE, ]
  statehospitals <- subset(statehospitals, State %in% state, select = c("Hospital.Name", validOutcomes[[outcome]]))
  statehospitals[, 2] <- as.numeric(statehospitals[, 2])
  
  # assign new names to columns
  names(statehospitals) <- c("Hospital.Name", "Rate")
  
  # find rows with AN's and remove them  
  good <- complete.cases(statehospitals)
  statehospitals <- statehospitals[complete.cases(statehospitals), ]
  
  # order the data frame 1. Hospital.Name then Rate
  sortedNames <- statehospitals[order(statehospitals$Hospital.Name, decreasing = FALSE), ]
  sorted <- sortedNames[order(sortedNames$Rate, decreasing = FALSE), ]
  
  
  # verify num term "best", "worst" or integer value
  if (num == "best") {
    hospitalname <- sorted[1, ]$Hospital.Name
  } else if (num == "worst") {
    hospitalname <- sorted[nrow(sorted), ]$Hospital.Name
  } else {
    hospitalname <- sorted[as.numeric(num), ]$Hospital.Name  
  }
  
  hospitalname
  
}