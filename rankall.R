rankall <- function(outcome, num = "best") {
hosp <- data[, c("Hospital.Name", "State", validOutcomes[[outcome]])]

# assign new names to columns
names(hosp) <- c("hospital", "state", "rate")
hosp[, 3] <- as.numeric(hosp[, 3])

# find rows with AN's and remove them  
good <- complete.cases(hosp)
hosp <- hosp[complete.cases(hosp), ]  

splitted <- split(hosp, hosp$state)
hospitals <- data.frame()

for (dat in splitted) {
  
  # order the Names prior to the Rates increasingly 
  datName <- dat[order(dat$hospital, decreasing = FALSE), ]
  dat <- datName[order(datName$rate, decreasing = FALSE), ]
  
  if (num == "best") {
    
    hospitals <- rbind(hospitals, dat[1, ])
  } else if (num == "worst") {
    
    hospitals <- rbind(hospitals,dat[nrow(dat), ])
  } else {
    
    if ( !is.na(dat[as.numeric(num), ]$hospital) ){
      
      hospitals <- rbind(hospitals,dat[as.numeric(num), ])  
    } else{
      
      hospitals <- rbind(hospitals, data.frame(hospital = NA, state = dat[1, "state"], rate = NA))
    }
    
    
  }
}

# Return a data frame with the hospital names and the abbreviated state name
hospitals <- hospitals[, c("hospital", "state")]
}