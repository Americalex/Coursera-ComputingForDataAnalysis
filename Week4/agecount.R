agecount <- function(age=NULL){
  if(is.null(age))
  {
    stop("cause has not been specified")
  }
  
  data <- readLines("homicides.txt")
  
  return(sum(grepl(paste("[^1-9]",age,"years"),data,ignore.case=TRUE)))
}