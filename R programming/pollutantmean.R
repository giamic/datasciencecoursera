pollutantmean <- function(directory, pollutant, id=1:332) {
	## pollutant can be either "sulfate" or "nitrate"
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
	sum <- 0
	n.measures <- 0
	for (i in id){
		if (i/10<1){
			text <- paste("/00", i, ".csv", sep="")
		}
		else if (i/100<1){
			text <- paste("/0", i, ".csv", sep="")
		}
		else text <- paste("/", i, ".csv", sep="")
	
		filename <- paste(directory, text, sep="")
		data <- read.csv(filename)
		temp.meas <- sum(!is.na(data[pollutant]))
		if (temp.meas != 0) {temp.sum <- sum(data[pollutant], na.rm=T)}
		else {temp.sum <- 0}			
		sum <- sum + temp.sum
		n.measures <- n.measures + temp.meas
	}
	sum / n.measures
}
