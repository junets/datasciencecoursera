source('complete.R')
corr<- function(directory, threshold = 0) {
  df <- list.files(path = directory , full.names = T)
  good <- subset(complete(directory,1:332), nobs > threshold)
  result <- vector(mode = "numeric", length = 0) 
  
  for(i in good$id) {
    ok <- na.omit(read.csv(df[i]))
    vec <- cor(ok$sulfate, ok$nitrate)
    result <- append(result,vec)
  }
  return(result)
}
