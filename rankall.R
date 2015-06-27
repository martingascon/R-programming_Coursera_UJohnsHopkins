
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cols<-c(2,7,11,17,23)
  data<-file[,cols]

  ## Check that outcome is valid
  outcomes<-c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, outcomes))
    stop("invalid outcome")   
  
  ## Check that num is valid
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid num")
    }
  }
  # select the correct outcome  (1st: Hospital Name, 2nd: State, 3rd: Mort. Rate)
  if (outcome=="heart attack")     data_set<-subset(data[,c(1:3)])         
  if (outcome=="heart failure")    data_set<-subset(data[,c(1,2,4)])          
  if (outcome=="pneumonia")        data_set<-subset(data[,c(1,2,5)])  
  
  data_set[, 3] = suppressWarnings( as.numeric(data_set[, 3]) )   ## convert to numeric
  data_set = data_set[!is.na(data_set[,3]),]                     ## remove NA
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the (abbreviated) state name
  splited = split(data_set, data_set[,2])                          ## split by states

  rank<-function(set, num){
    set = set[order(set[,3], set[,1]),]   # order by rate and hospital name
    if(class(num) == "character") {
      if(num == "best")  return (set[,1][1])
      if(num == "worst") return (set[,1][nrow(set)])
    }
    else return (set[,1][num])
  }
  result = lapply(splited, rank, num)         # loop over the data with rank FUN and num
  return (data.frame(hospital=unlist(result),state=names(result)))  #return data frame
}
