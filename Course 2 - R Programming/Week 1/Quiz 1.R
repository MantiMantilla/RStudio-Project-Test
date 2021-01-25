library(readr)
quiz.data <- read_csv("Data/hw1_data.csv")

head(quiz.data)

nrow(quiz.data)

quiz.data[(nrow(quiz.data)-1):nrow(quiz.data),]

quiz.data$Ozone[47]

sum(is.na(quiz.data$Ozone))

mean(quiz.data$Ozone, na.rm=TRUE)

mean(quiz.data[quiz.data$Ozone > 31 & quiz.data$Temp > 90, ]$Solar.R, na.rm = TRUE)

mean(quiz.data[quiz.data$Month == 6, ]$Temp, na.rm = TRUE)

max(quiz.data[quiz.data$Month == 5 & complete.cases(quiz.data$Ozone), ]$Ozone)


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


my_vector <- makeVector( c(3,5,6,8,2,4))
my_vector$set(rev(my_vector$get()))
my_vector$get()


