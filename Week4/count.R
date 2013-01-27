count <- function(cause=NULL){
  if(is.null(cause))
  {
    stop("cause has not been specified")
  }
  if(cause == "asphyxiation"
     || cause =="blunt force"
     || cause =="other"
     || cause =="shooting"
     || cause =="stabbing"
     || cause =="unknown")
  {
    data <- readLines("homicides.txt")
    
    return(sum(grepl(paste("Cause:",cause),data,ignore.case=TRUE)))
  }
  stop("specified cause is not allowed")
}