rankhospital <- function(state, outcome, num = "best"){
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
  
  if (outcome == "heart attack"){st<-st[order(st$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),] 
                                 ;data2 <-st[,2];}
  else if (outcome == "heart failure") {st<-st[order(st$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] ;data2 <-st[,2];}
  else {st<-st[order(st$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),];data2 <-st[,2];}
  
  l<-length(data2) ;
  if(num =="best") {ho<-(data2)[1];return(as.character(ho));}
  if(num =="worst") {ho<-(data2)[l];return(as.character(ho));}
  if(num>l) {ho<-NA; return (ho);}
   ho<-(data2)[num];
  ##ho<-sort(as.numeric(as.character(data2)))[1];
  return(as.character(ho))
  
  ##return(data2)
}