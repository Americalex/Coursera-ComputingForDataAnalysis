complete <- function(directory, id=1:332){
  
  t<-data.frame(id=2,nobs=1:length(id))
  j <- 1
  for(i in id)
  {
    filename = formatC(as.numeric(i), flag="0", width=3)
    t[j,]<-c(i, sum(complete.cases(read.csv(paste0(directory,"/",filename,".csv")))))    
    j<-j+1
  }
  return(t)
}