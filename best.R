best <- function(state, outcome) {
	## Read outcome data
	#state<-"TX"
	#outcome<-"heart attack"
	setwd("/Users/porters/Documents/Experimentation/Coursera/rprog-005/Prog3/rprog-data-ProgAssignment3-data")
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	states<-outcome_data$State
	states<-states[!duplicated(states)]
	state_num<-match(state,states)
	outcomes<-c("heart attack","heart failure","pneumonia")
	outcome_num<-match(outcome,outcomes)
	outcome_columns<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	## Check that state and outcome are valid
	if(is.na(state_num)){
			stop("invalid state")
		}
	if(is.na(outcome_num)){
			stop("invalid outcome");		
		} 
	subset_data<-subset(outcome_data,State==state)
	## Return hospital name in that state with lowest 30-day death rate
	mortality_vector<-as.numeric(subset_data[,outcome_columns[outcome_num]])
	return(subset_data$Hospital.Name[which(rank(mortality_vector,ties.method='min')==1)])
}
