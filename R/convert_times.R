#'Convert the time points into time since the epidemic began
#'
#'For a given epidemic and time points, return an array of numeric time (in seconds) since the epidemic started \cr
#'
#'@usage convert_times(epidemic, time_points)
#'@param epidemic: An Epidemic object
#'@param time_points: A vector of time points outputted by GMF (of format %Y-%m-%dT%H:%M:%OS")
#'@return timings: a vector whose ith element describes the numeric time since the epidemic began (in seconds)
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'recovery_times <- my_epi$hosts[[1]]$host_switch_time[which(my_epi$hosts[[1]]$host_history=='Recovered')] # get the recovery times of host 1, for instance
#'recovery_times_since_epidemic_start <- convert_times(my_epi, recovery_times)
#'
#' #examine the output:
#' recovery_times_since_epidemic_start
#'@export

convert_times <- function(epidemic, time_points)
	{
	options(digits.secs=6)
	timings <- c()
	start_time <- min(unlist(lapply(epidemic$epidemic_network,'[[',1))) # time of first infection
	for (i in 1:length(time_points))
		{
		timings[i] <- difftime(as.POSIXct(time_points[i],format="%Y-%m-%dT%H:%M:%OS"), as.POSIXct(start_time,format="%Y-%m-%dT%H:%M:%OS"))
		}
	return(timings)
	}

#'Convert the infection time points into time since the epidemic began
#'
#'For a given epidemic, return an array of numeric times of infection since the epidemic started \cr
#'
#'@usage convert_infection_times(epidemic)
#'@param epidemic: An Epidemic object
#'@return timings: a vector whose ith element describes the numeric time since the epidemic began (in seconds) in which an infection event occurred
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'infection_times_since_epidemic_start <- convert_infection_times(my_epi, infection_times)
#'
#' #examine the output:
#' infection_times_since_epidemic_start
#'@export

convert_infection_times <- function(epidemic)
	{
	options(digits.secs=6)
	timings <- c()
	start_time <- min(unlist(lapply(epidemic$epidemic_network,'[[',1))) # time of first infection
	time_points <- unlist(lapply(epidemic$epidemic_network,'[[',1))
	for (i in 1:length(time_points))
		{
		timings[i] <- difftime(as.POSIXct(time_points[i],format="%Y-%m-%dT%H:%M:%OS"), as.POSIXct(start_time,format="%Y-%m-%dT%H:%M:%OS"))
		}
	return(timings)
	}
