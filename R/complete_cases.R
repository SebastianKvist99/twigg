complete_cases <- function(dataset){
  data <- na.omit(dataset)
  if (nrow(dataset)<10){
    stop("Too few observations in data set ot perform meaningfull screening")
  }
}
