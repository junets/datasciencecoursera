rankall <- function(outcome, num = 'best'){
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    df <- data[,c(2,7,11,17,23)]
    colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    for(i in 3:5) df[,i] <- as.numeric(df[,i])
    if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    } else {
        tt <- split(df,df$state)
        result <- data.frame()
        for(i in 1:length(tt)){
            if(num == 'worst'){
                k <- tt[[i]][,c("hospital","state",outcome)]
                m <- k[,outcome]
                n <- max(m, na.rm = T)
                h <- k[,c('hospital','state')][which(k[,outcome] == n),]
                h <- h[order(h[,'hospital'], na.last = T, decreasing = F),][1,]
            } else if(num == 'best'){
                k <- tt[[i]][,c("hospital","state",outcome)]
                m <- k[,outcome]
                n <- min(m, na.rm = T)
                h <- k[,c('hospital','state')][which(k[,outcome] == n),]
                h <- h[order(h[,'hospital'], na.last = T, decreasing = F),][1,]
            } else{
                k <- tt[[i]][,c("hospital","state",outcome)]
                n <- k[order(k[,'hospital'], na.last = T, decreasing = F),]
                n <- n[order(n[,outcome], na.last = T, decreasing = F),]
                h <- n[num,c('hospital','state')]
            }
            rownames(h) <- h[,2] <- unique(tt[[i]][,'state'])
            result <- rbind(result, h)
        }
        
    }
    return(result)
}