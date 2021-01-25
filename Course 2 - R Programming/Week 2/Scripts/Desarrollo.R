pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutant.df <- tibble( Date=character(), sulfate=numeric(), nitrate=numeric(), ID=numeric())
  for(i in id) {
    if(i <= 9) {
      csv.name <- paste("00",i, sep="")
    } else if(i <= 99) {
      csv.name <- paste("0",i, sep="")
    } else {
      csv.name <- i
    }
    pollutant.df<-rbind(pollutant.df,read_csv(paste(directory, "/", csv.name, ".csv", sep="")))  
  }
  sum <- 0
  count <- 0
  for(j in unlist(pollutant.df[,pollutant])){
    if(!is.na(j)){
      sum <- sum + j
      count <- count + 1 
    }
  }
  return(sum/count)
}

complete <- function(directory, id=1:332) {
  files.df <- tibble( ID=numeric(), nobs=numeric())
  for(i in id) {
    if(i <= 9) {
      csv.name <- paste("00",i, sep="")
    } else if(i <= 99) {
      csv.name <- paste("0",i, sep="")
    } else {
      csv.name <- i
    }
    pollutant.df <- read_csv(paste(directory, "/", csv.name, ".csv", sep=""))
    compl.count <- complete.cases(pollutant.df$sulfate) & complete.cases(pollutant.df$nitrate)
    new.row <- tibble(ID=i, nobs=sum(compl.count))
    files.df <- rbind(files.df, new.row)
  }
return(files.df)
}

corr <- function(directory, threshold=0) {
  files.corr.vector <- numeric()
  for(i in 1:332) {
    if(i <= 9) {
      csv.name <- paste("00",i, sep="")
    } else if(i <= 99) {
      csv.name <- paste("0",i, sep="")
    } else {
      csv.name <- i
    }
    pollutant.df <- read_csv(paste(directory, "/", csv.name, ".csv", sep=""))
    compl.count <- complete.cases(pollutant.df$sulfate) & complete.cases(pollutant.df$nitrate)
    if (sum(compl.count) >= threshold) {
      sulfate.vector <- pollutant.df[compl.count,"sulfate"]
      nitrate.vector <- pollutant.df[compl.count,"nitrate"]
      files.corr.vector <- c(files.corr.vector, cor(sulfate.vector,nitrate.vector))
    }
  }
  return(files.corr.vector)
}

