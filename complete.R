complete <- function(directory, id = 1:332) {
  filesname <- list.files(path = directory, full.names = TRUE)
  df <- data.frame()
    for(i in id){
    df2 <- read.csv(filesname[i])
    df2 <- na.omit(df2)
    k <- c(i,nrow(df2))
    df <- rbind(df,k)
  }
  colnames(df) <- c('id','nobs')
  return(df)
}
