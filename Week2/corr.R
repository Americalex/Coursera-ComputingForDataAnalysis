corr <- function(directory, threshold = 0){
  ret <- vector()
  
  for(file in list.files(directory))
  {
    data <-read.csv(paste0(directory,"/",file))
    comp <- complete.cases(data)
    if ( sum(comp) > threshold)
    {
      ret <- append(ret,cor(data$nitrate[comp],data$sulfate[comp]))
    }
  }
  return(ret)
}