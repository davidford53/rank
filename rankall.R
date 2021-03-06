## this function will return a list of hospitals, one for each state, 
## of a specific rank.

## it does this by calling the previous function, rankhospital, for each state,
## for a given condition and rank.

rankall <- function(condition, num="best"){

  ## Read outcome data
  outcome<-read.csv("c:\\users\\dwight\\documents\\r_workspace\\week_4_data\\outcome-of-care-measures.csv", colClasses = "character")
 
   # get states in data
  states<-outcome$State
 
   # remove duplicates
  states<-states[!duplicated(states)]
 
   # sort the states
  states<-sort(states)
  
  
  ## Check that state and outcome are valid

  # if no records, issue error message
  
    # select which column to sort and choose from based on incoming condition
    # based on the defined layout of fields in the incoming table.
    col<-switch(condition,"heart attack"=11, "heart failure"=17, "pneumonia"=23)
    # throw error if outcome doesn't match list above.
    if(is.null(col)) stop("Invalid outcome")
    else {
      
      # create output variable
      state_rank<-c()
      
      #populate output variable
      for(i in seq_along(states)){
        # call previous rankhospital function and loop through states
        state_rank<-rbind(state_rank,c(rankhospital(states[i], condition, num), states[i]))
      
      }
     # name columns
     colnames(state_rank)<-c("hospital","state")
     state_rank
    }
  
  
}
# 
# > head(rankall("heart attack",20),10)
# hospital                              state
# [1,] NA                                    "AK" 
# [2,] "D W MCMILLAN MEMORIAL HOSPITAL"      "AL" 
# [3,] "ARKANSAS METHODIST MEDICAL CENTER"   "AR" 
# [4,] "JOHN C LINCOLN DEER VALLEY HOSPITAL" "AZ" 
# [5,] "SHERMAN OAKS HOSPITAL"               "CA" 
# [6,] "SKY RIDGE MEDICAL CENTER"            "CO" 
# [7,] "MIDSTATE MEDICAL CENTER"             "CT" 
# [8,] NA                                    "DC" 
# [9,] NA                                    "DE" 
# [10,] "SOUTH FLORIDA BAPTIST HOSPITAL"      "FL"

# > tail(rankall("pneumonia","worst"),3)
# hospital                                     state
# [52,] "MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC" "WI" 
# [53,] "PLATEAU MEDICAL CENTER"                     "WV" 
# [54,] "NORTH BIG HORN HOSPITAL DISTRICT"           "WY"
# 
# > tail(rankall("heart failure"),10)
# hospital                                                            state
# [45,] "WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL"                         "TN" 
# [46,] "FORT DUNCAN MEDICAL CENTER"                                        "TX" 
# [47,] "VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER" "UT" 
# [48,] "SENTARA POTOMAC HOSPITAL"                                          "VA" 
# [49,] "GOV JUAN F LUIS HOSPITAL & MEDICAL CTR"                            "VI" 
# [50,] "SPRINGFIELD HOSPITAL"                                              "VT" 
# [51,] "HARBORVIEW MEDICAL CENTER"                                         "WA" 
# [52,] "AURORA ST LUKES MEDICAL CENTER"                                    "WI" 
# [53,] "FAIRMONT GENERAL HOSPITAL"                                         "WV" 
# [54,] "CHEYENNE VA MEDICAL CENTER"                                        "WY" 
