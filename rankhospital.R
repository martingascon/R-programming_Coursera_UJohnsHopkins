rankhospital <- function(state, outcome, num = "best") {

  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cols<-c(2,7,11,17,23)
  data<-file[,cols]
  
  ## Check that state and outcome are valid
  states<-levels(factor(file[,7]))
  if (!is.element(state, states))
    stop("invalid state")
  
  
  
  outcomes<-c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, outcomes))
    stop("invalid outcome")  
  
  # select the correct outcome  (1st: Hospital Name, 2nd: State, 3rd: Mort. Rate)
  if (outcome=="heart attack")     data_set<-subset(data[,c(1:3)])         
  if (outcome=="heart failure")    data_set<-subset(data[,c(1,2,4)])          
  if (outcome=="pneumonia")        data_set<-subset(data[,c(1,2,5)])  
  
  data_set[,3] = suppressWarnings( as.numeric(data_set[,3]) )     ## convert to numeric
  data_set <-subset(data_set, data_set[,2]==state)                ## select those within state
  data_set = data_set[!is.na(data_set[,3]),]                     ## remove NA
  data_set <-data_set[order(data_set[,3], data_set[,1]),1:3]   # order the data 
  
  if (class(num) == "character"){
    if (num == "best")  rank<-1
    if (num == "worst") rank<-nrow(data_set)
  }
  else {rank<-num}
  
  if (rank>nrow(data_set))
    return("NA")
  else 
    return(data_set[,1][rank])              # print the name of the hospital in rank position 
}