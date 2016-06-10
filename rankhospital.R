
rankhospital <- function(state, condition, num="best"){

  ## Read outcome data
  outcome<-read.csv("c:\\users\\dwight\\documents\\r_workspace\\week_4_data\\outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  cnt<-nrow(subset(outcome,State== state))
  # if no records, issue error message
  if(cnt==0) 
    stop ("Invalid State")
  
  else
  {
    # select which column to sort and choose from based on incoming condition
    # based on the defined layout of fields in the incoming table.
    col<-switch(condition,"heart attack"=11, "heart failure"=17, "pneumonia"=23)
    # throw error if outcome doesn't match list above.
    if(is.null(col)) stop("Invalid outcome")
    else {
      
      # get Hospital.Name, State, and whatever mortality rate was chosen by the command line
      outcome3<-outcome[,c(2,7, col)]
      outcome3<-subset(outcome3,State == state)
      colnames(outcome3)<-c("Hospital_Name","State","Rate")
      outcome3<-subset(outcome3,Rate != "Not Available")
      
      #converting rate from text to number
      outcome3[,3]<-as.numeric(outcome3[,3])
      
      # sort results by that Rate column
      outcome3<-outcome3[order(outcome3$Rate, outcome3$Hospital_Name),]
      
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      if(num=="best") num<-1
      if(num=="worst") num<-nrow(outcome3)
      if(num> nrow(outcome3)) NA
      else  outcome3[num ,1]
    }
  }
  
}

# > rankhospital("TX","heart failure",4)
# [1] "DETAR HOSPITAL NAVARRO"