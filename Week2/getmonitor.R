getmonitor <- function (id, directory, summarize=FALSE){
  
  id = formatC(as.numeric(id), flag="0", width=3)
  x <- read.csv(paste0(directory,"/",id,".csv"))
  
  if(summarize)
  {
    print(summary(x))
  }
  
  return(x) 
}