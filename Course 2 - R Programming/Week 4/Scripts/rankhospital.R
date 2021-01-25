rankhospital <- function(state, outcome.param, num = "best") {
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
  data <- outcome[outcome$State == state & complete.cases(outcome[outcomeCols[outcome.param]]), c(outcomeCols[outcome.param], "Hospital.Name") ]
  
  data <- data[order(data[, outcomeCols[outcome.param]], data$Hospital.Name), ]
  
  rownames(data) <- NULL
  
  ## Return hospital name in that state with the given rank 30-day death rate
  if (num == "best") {
    return(data[1, "Hospital.Name"])
  } else if (num == "worst") {
    return(data[length(data[,1]), "Hospital.Name"])
  } else if (is.numeric(num)) {
    return(data[num, "Hospital.Name"])
  }
  
}