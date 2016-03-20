library(dplyr)
library(ggplot2)

hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Change the class of the columns containing data about 30 day motality rates to numeric
hospital_data[, 11] <- as.numeric(hospital_data[, 11])
hospital_data[, 17] <- as.numeric(hospital_data[, 17])
hospital_data[, 23] <- as.numeric(hospital_data[, 23])

## This function takes as input two arguments - state name and outcome.
## It checks of the name of state and the outcome are both valid.
## It returns the name of the hospital with lowest mortality rate for the given state.

best <- function(state, outcome) {
	State_hospitals <- (subset(hospital_data, State == state))
	
	## Checking validity of the input state
	if (nrow(State_hospitals) == 0) {
		stop ("invalid state")
	}
	
	## Checking validity of the input outcome
	if (!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
		stop ("invalid outcome")
	}
	
	## Depending on the state and the outcome, select the best hospital for a particular condition
	if (outcome == "heart attack"){
		minimum_mortality <- min(State_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE)
		Best_hospital <- na.omit(State_hospitals$Hospital.Name[State_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minimum_mortality])
		Best_hospital <- Best_hospital[!is.na(Best_hospital)]
	}
	
	if (outcome == "heart failure"){
		minimum_mortality <- min(State_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)
		Best_hospital <- na.omit(State_hospitals$Hospital.Name[State_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minimum_mortality])
		Best_hospital <- Best_hospital[!is.na(Best_hospital)]
	}
	
	if (outcome == "pneumonia"){
		minimum_mortality <- min(State_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = TRUE)
		Best_hospital <- na.omit(State_hospitals$Hospital.Name[State_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minimum_mortality])
		Best_hospital <- Best_hospital[!is.na(Best_hospital)]
	}	
	
	## Checking if there are more than one hospital with same mortality rate; if yes, sort them alphabetically
	if (length(Best_hospital) > 1){
		Best_hospital <- sort(Best_hospital)
		}
	
	Best_hospital[1]	
}
