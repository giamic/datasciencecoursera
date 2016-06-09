rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("./outcome-of-care-measures.csv", stringsAsFactors=FALSE)
	possible.outcomes <- c("heart attack", "heart failure", "pneumonia")

	## Check that outcome is valid
	if (all(outcome != possible.outcomes)) {stop("invalid outcome")}

	## Choose which columns are interesting
	if (outcome=="heart attack") {choose <- 11}     #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	if (outcome=="heart failure") {choose <- 17}    #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	if (outcome=="pneumonia") {choose <- 23}        #"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	data.sub <- subset(data, select=c(2, 7, choose))
	colnames(data.sub) <- c("hospital", "state", outcome)

	## Tidy the data and remove unwanted rows
	data.sub[data.sub=="Not Available"] <- NA
	data.sub <- data.sub[complete.cases(data.sub), ]
	data.sub[, outcome] <- as.numeric(as.character(data.sub[, outcome]))

	## Order the data frames by state, best outcome, and, in case of tie, name
	data.ord <- data.sub[order(data.sub[, "state"], data.sub[, outcome], data.sub[, "hospital"]), ]

	## Return data frame [State, Hospital.Name] with the given rank of 30-day death rate
	res <- data.frame(hospital=character(), state=character(), stringsAsFactors = FALSE)
	data.split <- split(data.ord, data.ord$state)

	if (num == "best") { 
		temp <- 1
	}
	for (idx in seq_along(data.split)){
		data.state <- data.split[[idx]]
		if (num == "worst") {
			temp <- nrow(data.state)
		}
		if (is.numeric(num) & num > nrow(data.state)){
			res[idx,] <- c(NA, data.state$state[1])
		} else {
			res[idx,] <- c(data.state[temp, "hospital"], data.state[temp, "state"])
		}
	}
	rownames(res) <- res$state
	res
}
