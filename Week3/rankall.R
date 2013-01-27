rankall <- function(outcome, num = "best")
{
  if(num == "best") num = 1
  if(num > 0 || num =="worst")
  {
    data <- read.csv("outcome-of-care-measures.csv")

    if(outcome =="heart attack") column = 11
    else if(outcome =="heart failure") column = 17
    else if(outcome =="pneumonia") column = 23
    else stop("invalid outcome")
    
    data[,column] <- as.numeric(as.character(data[,column]))
    filtered <- complete.cases(data[,column])
    filtered_data <- data[filtered,]
    
    
    answer <-sapply(
      split(filtered_data,filtered_data[,7]),
      function(x){
        ranking <- data.frame(x[
          order(x[,column], x[,2])
          ,c(2,7)],stringsAsFactors=FALSE)
        size = nrow(ranking)
        
        if(num == "worst") return(c(hospital=ranking[size,1],state=as.character(ranking[size,2])))
        if(size < num) return(c(hospital=NA,state=as.character(ranking[1,2])))
        return(c(hospital=as.character(ranking[num,1]),state=as.character(ranking[num,2])))
      }
      )
    return(as.data.frame(t(answer)))
  }
  
  stop("num must be positiv integer")
  
}