rankhospital <- function(state_name, outcome_name, num = "best") {
  ## Read outcome data
  data <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  # Check for validity
  states = table(data$State)
  state_names <- names(states)
  index <- state_names == state_name
  f <- sum(index)
  
  if (f == 0) {
    stop("invalid state")
  }
  
  # Check for outcome validity
  repo <- c("heart attack", "heart failure", "pneumonia")
  index <- repo == outcome_name
  found <- sum(index)
  
  if (found == 0) {
    stop("invalid outcome")
  }
  
  ## Find the best hospital of a certain outcome in a certain state
  
  # keep only the hospitals from the desired state
  index <- data$State == state_name
  hospitals <- data[index,]
  
  if (outcome_name == "heart attack") {
    index <- 11
  }else if (outcome_name == "heart failure") {
    index <- 17
  }else{
    index <- 23
  }
  
  # Keep only the name and the desired outcome columns
  hospital <- hospitals[,c(2,index)]
  
  # Remove NAs
  index <- is.na(as.numeric(hospital[,2]))
  hospital <- hospital[!index,]
  
  # Order the hospitals (descenting)
  hospital <- hospital[order(hospital$Hospital.Name),]
  if (num == "worst") {
    hospital <-
      hospital[order(as.numeric(hospital[,2]), decreasing = TRUE),]
  }else{
    hospital <- hospital[order(as.numeric(hospital[,2])),]
  }
  
  print(min(as.numeric(hospital[,2])))
  ## Return hospital name in that state with the given rank
  if (num <= nrow(hospital)) {
    return(hospital[num, 1])
    return(hospital)
  }else{
    return(NA)
  }
}