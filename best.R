best <- function(state, outcome)  {
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##Rate <- as.numeric(out[out[,11] != "Not Available", 11])
  
  out[, 11] <- as.numeric(out[, 11])
  out[, 17] <- as.numeric(out[, 17])
  out[, 23] <- as.numeric(out[, 23])
  if((state %in% names(table(out$State))) == FALSE)
    stop("invalid state")
  
  if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE)
    stop("invalid outcome")
  
  st<-out[out[,7]==state,]
  
  if (outcome == "heart attack"){data2 <- ho<-st[,2][st[,11]==sort(st[,11])[1]]}
  else if (outcome == "heart failure") { data2 <- ho<-st[,2][st[,17]==sort(st[,17])[1]]}
  else {data2 <- ho<-st[,2][st[,23]==sort(st[,23])[1]]}
  
  ho<-sort(data2)[1];
  ##ho<-sort(as.numeric(as.character(data2)))[1];
  return(as.character(ho))
  
  ##return(data2)
}