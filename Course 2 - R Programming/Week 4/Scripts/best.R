best <- function(state, outcome.param) {
  outcomeCols <- colnames(outcome)[c(11, 17, 23)]
  names(outcomeCols) <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if (!state %in% levels(as.factor(outcome$State))) {
    stop("invalid state")
  }
  if (!outcome.param %in% names(outcomeCols)) {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  data <- outcome[outcome$State == state, c(outcomeCols[outcome.param], "Hospital.Name") ]
  
  ## Return hospital name in that state with lowest 30-day death rate
  return(data[which.min(data[ , outcomeCols[outcome.param]]), "Hospital.Name"])
  
}
