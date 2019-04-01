#' A function for calculating the mortality rate
#'For a given Epidemic object, determine the mortality rate over a user-specified time interval\cr
#'@usage calculate_mortality_rate(epidemic, t1, t2)
#'@param epidemic An Epidemic object
#'@param t1 a value denoting the beginning of the  time interval of interest
#'@param t2 a value denoting the end of the time interval of interest
#'@return a numeric value representing the mortality rate from the pathogen between time t1 and time t2.
#'@details The mortality rate describes the proportion of hosts that die during a particular time interval from the infection
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_mortality_rate(my_epi, 4, 7)
#'@export

calculate_mortality_rate <- function(epidemic, t1, t2)
	{
	timepoints <- reconstruct_epidemic_history(epidemic)$time_elapsed
	epidemic_summary <- summarize_trajectory(epidemic)
	Ninds <- numeric()
	N_dead <- numeric()
	mortality_rate <- numeric()

	tMin <- min(which(timepoints > t1))
	tMax <- max(which(timepoints < t2))

	Ninds <- numeric()
	N_dead <- numeric()
	mortality_rate <- numeric()

	return((epidemic_summary['Dead',tMax] - epidemic_summary['Dead',tMin])/(tMax-tMin))
	}
	
