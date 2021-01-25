rankall <- function(outcome.param, num = "best") {
  outcomeCols <- colnames(outcome)[c(11, 17, 23)]
  names(outcomeCols) <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!outcome.param %in% names(outcomeCols)) {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  data <- outcome[, c("State", outcomeCols[outcome.param], "Hospital.Name") ]
  
  result <- data.frame( hospital = character(), state = character())
  
  if (num == "best") {
    for (i in levels(as.factor(outcome$State))) {
      append <- data[data$State == i, ]
      append <- append[order(append[, 2], append[, 3]), ]
      append <- append[complete.cases(append[, 2]), ]
      append <- append[1, c(3, 1)]
      if (is.na(append[1, 2])) {
        append[1, 2] <- i
      }
      result <- rbind(result, append)
    }
  } else if (num == "worst") {
    for (i in levels(as.factor(outcome$State))) {
      append <- data[data$State == i, ]
      append <- append[order(append[, 2], append[, 3]), ]
      append <- append[complete.cases(append[, 2]), ]
      append <- append[length(append[, 1]), c(3, 1)]
      if (is.na(append[1, 2])) {
        append[1, 2] <- i
      }
      result <- rbind(result, append)
    }
  } else if (is.numeric(num)) {
    for (i in levels(as.factor(outcome$State))) {
      append <- data[data$State == i, ]
      append <- append[order(append[, 2], append[, 3]), ]
      append <- append[complete.cases(append[, 2]), ]
      append <- append[num, c(3, 1)]
      if (is.na(append[1, 2])) {
        append[1, 2] <- i
      }
      result <- rbind(result, append)
    }
  }
  
  rownames(result) <- NULL
  
  ## Return hospital name in that state with the given rank 30-day death rate

  
  ## For each state, find the hospital of the given rank
  
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
  return(result)
}