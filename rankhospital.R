rankhospital <- function(state, outcome, num = 'best'){
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    df <- data[,c(2,7,11,17,23)]
    colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    for(i in 3:5) df[,i] <- as.numeric(df[,i])
    if(!any(state == df[,'state'])) {
        stop("invalid state")
    } else if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    } else if(num == 'worst'){
        index <- which(state == df$state)
        k <- df[index,]
        k <- k[,c("hospital","state",outcome)]
        m <<- k[,outcome]
        n <<- max(m, na.rm = T)
        h <<- k[,'hospital'][which(k[,outcome] == n)]
    } else if(num == 'best'){
        index <- which(state == df$state)
        k <- df[index,]
        k <- k[,c("hospital","state",outcome)]
        m <<- k[,outcome]
        n <<- min(m, na.rm = T)
        h <<- k[,'hospital'][which(k[,outcome] == n)]
    } else{
        index <- which(state == df$state)
        k <- df[index,]
        k <- k[,c("hospital","state",outcome)]
        n <<- k[order(k[,'hospital'], na.last = T, decreasing = F),]
        n <<- n[order(n[,outcome], na.last = T, decreasing = F),]
        h <<- n[num,'hospital']
    }
    return(h)
}