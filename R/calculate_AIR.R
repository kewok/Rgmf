#' A function for calculating the apparent infection rate
#'For a given Epidemic object, determine the apparant infection rate over two time steps \cr
#'@usage calculate_AIR(epidemic, t1, t2)
#'@param epidemic An Epidemic object
#'@param t1 an integer value denoting the beginning of the time interval of interest. The integer value should correspond to an updating step of the GMF epidemic.
#'@param t2 an integer value denoting the end of the time interval of interest.  The integer value should correspond to an updating step of the GMF epidemic.
#'@return a numeric value representing the apparent death rate from the epidemic between t1 and t2.
#'@details The apparent infection rate is perhaps among the simplest of the various quantities measuring epidemic growth. It is simply a crude estimate of the growth rate of the proportion of infected individuals. The general formula is simply log((p (1-p'))/(p'(1-p)))/T, where T is unit time, p is the current proportion of infected individuals, and p' is the proportion of infected individuals T time units ago.
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_AIR(my_epi,5,35)
#'@export

calculate_AIR <- function(epidemic, t1, t2)
	{
	if (t1==t2)
		{
		return(print("Error calculating apparent infection rate: Time points t1 and t2 must be distinct"))
		}
	timepoints <- reconstruct_epidemic_history(epidemic)$time_elapsed
	epidemic_summary <- summarize_trajectory(epidemic)
	Ninds <- numeric()
	N_carriers <- numeric()
	compartments <- row.names(epidemic_summary)
	dead_row <- which(compartments=='Dead')
	carriers <- which(compartments=='Infectious Symptomatic' | compartments=='Infectious Asymptomatic' | compartments=='Exposed symptomatic' | compartments=='Exposed Asymptomatic' | compartments == 'Isolated')

	for (i in 1:ncol(epidemic_summary))
		{
		Ninds[i] <- sum(epidemic_summary[row.names(epidemic_summary)[-dead_row],i])
		N_carriers[i] <- sum(epidemic_summary[carriers,i])
		}
	sicks <- N_carriers/Ninds
	if (sicks[t2]==1)
		{
		return(print("All hosts are infected; use a smaller t2 step to calculate the apparent infection rate"))
		}
	odds_ratio <- (sicks[t2]*(1-sicks[t1]))/(sicks[t1]*(1-sicks[t2]))
	apparent_infection_rate <- (1/(timepoints[t2]-timepoints[t1]))*log(odds_ratio)
	return(apparent_infection_rate)
	}

# This quantity is closely related to the odds ratio. An interesting exercise would be to have different students calculate the p term at various time points of the epidemic (again by interviewing their class mates), and then draw the change in the apparent infection rate over time. 
