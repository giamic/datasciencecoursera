rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("./outcome-of-care-measures.csv")
	possible.outcomes <- c("heart attack", "heart failure", "pneumonia")

	## Check that outcome is valid
	if (all(outcome != possible.outcomes)) {stop("invalid outcome")}

	## Choose which columns are interesting
	if (outcome=="heart attack") {choose <- 11}     #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	if (outcome=="heart failure") {choose <- 17}    #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	if (outcome=="pneumonia") {choose <- 23}        #"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	data.sub <- subset(data, select=c(2, 7, choose))
	colnames(data.sub) <- c("State", "Hospital.Name", outcome)

	## Tidy the data and remove unwanted rows
	data.sub[data.sub=="Not Available"] <- NA
	data.sub <- data.sub[complete.cases(data.sub), ]
	data.sub[, outcome] <- as.numeric(as.character(data.sub[, outcome]))

	## Order the data frames by state, best outcome, and, in case of tie, name
	data.ord <- data.sub[order(data.sub[, "State"], data.sub[, outcome], data.sub[, "Hospital.Name"]), ]

	## Choose the rank for every state
	data.split <- split(data.ord, data.ord$State)
	n.states <- length(data.split)
	if (num == "best") {
		num = rep.int(1, n.states)
	} else if (num == "worst") {
		num = nrow(data.split)
	} else {
		num <- rep.int(num, n.states)
		num[num>sapply(data.split, nrow)] <- NA	
	}
	

	## Return data frame [State, Hospital.Name] with the given rank of 30-day death rate
	res <- sapply(c(data.split[, "State"], data.split[, "Hospital.Name"]), "[[", num)
	res <- data.frame("State"=character(), "Hospital.Name"=character())
	for (i in seq_along(data.split)) {
		res[i, ] <- data.split[[i]][num[i], c("State", "Hospital.Name")]
	}
	res <- data.split[[num,c("State", "Hospital.Name")]]
	res
}
