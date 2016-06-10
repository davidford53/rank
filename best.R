## This program reads the outcome file, determines which 30 day mortality rate
## to use, based on the designated outcome, filters down the records to the
## designated state, sorts data frame ascending by the mortality rate figure
## and returns the first records at the top of the sorted results.

best <- function(state, condition){
  
  
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
        outcome3<-outcome3[order(outcome3$Rate),]

      
        ## Return hospital name in that state with lowest 30-day death ## rate
        outcome3[1,1]
      }
    }
}

# > source('~/R_Workspace/Week_4_Data/best.R')
# > best("TX","heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX","heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD","heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD","pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB","heart attack")
# Error in best("BB", "heart attack") : Invalid State
# > best("NY","hert attack")
# Error in best("NY", "hert attack") : Invalid outcome