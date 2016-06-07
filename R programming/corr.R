corr <- function(directory, threshold = 0) {
	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations between sulfate and nitrate
	## NOTE: Do not round the result!
	id <- which(complete(directory)$nobs > threshold)
	corr <- vector(mode="numeric", length=length(id))
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
		corr[idx] <- cor(data$sulfate, data$nitrate, use='complete.obs')
	}
	return(corr)
}
