best <- function(state, outcome) {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states<-as.list(file[, 7])
  if (!is.element(state, states))
    stop("invalid state")
  
  outcomes<-c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, outcomes))
     stop("invalid outcome")       
    
  ## Return hospital name in that state with lowest 30-day death rate
  cols<-c(2,7,11,17,23)
  data<-file[,cols]
  data[,3]<- suppressWarnings(as.numeric(data[,3]))      # made data as numeric
  data[,4]<- suppressWarnings(as.numeric(data[,4]))      # made data as numeric 
  data[,5]<- suppressWarnings(as.numeric(data[,5]))      # made data as numeric
  colnames(data)<-c("hospital","State","heart attack", "heart failure", "pneumonia")

  
  if (outcome=="heart attack")
    data_state<-subset(data[,c(1:3)], data$State==state)       # select the state    
  if (outcome=="heart failure")
    data_state<-subset(data[,c(1,2,4)], data$State==state)       # select the state    
  if (outcome=="pneumonia")
    data_state<-subset(data[,c(1,2,5)], data$State==state)       # select the state    
  
    data_state <- data_state[order(data_state[,3], data_state[,1], na.last=TRUE),1:3] # order the data 
    return(data_state[,1][1]) # print the name of the first hospital 
}
