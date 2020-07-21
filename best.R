best <- function(state, outcome) {
  ## Read outcome data
  
  
  ## Check that state and outcome are valid
  validOutcomes = list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
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
  # good <- complete.cases(statehospitals)
  
  statehospitals <- statehospitals[complete.cases(statehospitals), ]
  with(statehospitals, Hospital.Name[which.min(statehospitals[, 2])])
  
}
