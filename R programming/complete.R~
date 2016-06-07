complete <- function(directory, id = 1:332){
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	nobs <- vector(mode="integer", length=length(id))
	for (idx in seq_along(id)){
		i <- id[idx]		
		if (i/10<1){
			text <- paste("/00", i, ".csv", sep="")
		}
		else if (i/100<1){
			text <- paste("/0", i, ".csv", sep="")
		}
		else text <- paste("/", i, ".csv", sep="")
	
		filename <- paste(directory, text, sep="")
		data <- read.csv(filename)
		nobs[idx] <- sum(complete.cases(data))
	}
	data.frame(id, nobs)
}
