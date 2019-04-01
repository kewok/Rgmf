#' A function for calculating the force of infection
#'
#'For a given Epidemic object, determine the force of infection over time \cr
#'@usage calculate_FOI(epidemic)
#'@param epidemic An Epidemic object
#'@return a list consisting of the vector 'FOI', whose ith element represents the force of infection at a given time point of the epidemic, and the vector 'times' whose ith element corresponds to the timing (in seconds since epidemic inception) of the ith time point.
#'@details The force of infection is defined as the rate at which susceptible individuals aquire an infectious disease. In the standard epidemiological models used by GMF, this is simply the total rate at which individuals leave the susceptible compartment due to infection, i.e., the transmission rate multiplied either by the number of infectious hosts (if density-dependent transmission is assumed) or by the fraction of infectious hosts (if frequency-dependent transmission is assumed). \cr
#' A useful exercise would be to have each student determine the time they spent as a susceptible before they became infected. Then they could draw a histogram of this time as a class. Students will also be assigned to figure out the number of infectious individuals over time (through asking their colleagues). Then they will have to calculate this quantity manually. \cr
#
#' The force of infection changes over time as more individuals become infectious.  It would be interesting to see how the distribution of time spent as susceptibles before first infection shifts as the force of infection increases. \cr
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_FOI(my_epi)
#'@export


calculate_FOI <- function(epidemic)
	{
	options(digits.secs=6)
	counts <- summarize_trajectory(epidemic)
	FOI <- c()
	if (epidemic$epidemic_parameters$transmission_mode=='FD')
		{
		for (i in 1:ncol(counts))
			{
			FOI[i] <- epidemic$epidemic_parameters$transmission_rate * (counts['Infectious Asymptomatic',i]+counts['Infectious Symptomatic',i])/(sum(counts[,i])-counts['Dead',i])
			}
		}
	else
		{
		for (i in 1:ncol(counts))
			{
			FOI[i] <- epidemic$epidemic_parameters$transmission_rate * (counts['Infectious Asymptomatic',i]+counts['Infectious Symptomatic',i])
			}
		}
	timings <- reconstruct_epidemic_history(epi)$time_elapsed

	return(list(FOI=FOI,times=timings))
	}


