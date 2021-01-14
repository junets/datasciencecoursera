pollutantmean <- function(directory, pollutant, id = 1:332) {
  filesname <- list.files(path = directory, full.names = TRUE)
  df <- data.frame()
  for(i in id){
    df2 <- read.csv(filesname[i])
    df <- rbind(df,df2)
  }
  return(mean(df[, pollutant], na.rm = TRUE))
}
