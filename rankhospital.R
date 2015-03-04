rankhospital <- function(state, outcome,num="best") {
	## Read outcome data
	#state<-"MD"
	#outcome<-"heart attack"
	#num<-"worst"
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
	subset_data[,outcome_columns[outcome_num]]<-as.numeric(subset_data[,outcome_columns[outcome_num]])
	if (num=="best"){
			sorted_data<-subset_data[order(subset_data[,outcome_columns[outcome_num]],subset_data[,"Hospital.Name"]),]
			rank_num=1
		} else if (num=="worst"){
			sorted_data<-subset_data[order(-subset_data[,outcome_columns[outcome_num]],subset_data[,"Hospital.Name"]),]
			rank_num=1
		} else{
			sorted_data<-subset_data[order(subset_data[,outcome_columns[outcome_num]],subset_data[,"Hospital.Name"]),]
			rank_num<-as.numeric(num)
		}
	return(sorted_data$Hospital.Name[rank_num])
}
