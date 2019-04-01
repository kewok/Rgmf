#' A function for calculating the disease-adjust life years (here, units time)
#'
#'For a given Epidemic object, determine the disease adjusted life years (DALYs) for each host (i.e., host) \cr
#'@usage calculate_DALY(epidemic)
#'@param epidemic An Epidemic object
#'@return a vector whose i-th element represents the DALY of the i-th host
#'@details Disease-adjusted life years (DALYs) measures the number of years that are lost due to the combination of being sick and dying early before the expected life span. For GMF purposes, we can assume the expected life span is the duration of a GMF epidemic. \cr
#'A useful exercise would be to have each GMF time step set equal to one year, and assume that the first time step of the GMF exercise pertains to the student's current age. Then have each student calculate their individual DALY, with the life expectancy given for their country of origin and their sex. We can ask the student what the relationship would be between the day they died and their expected life span, and how that fits into the DALY calculation.\cr
#' Another interesting exercise might be to normalize DALY by the total number of time steps used, and express it as a fraction. This can help students appreciate how disease burden can increase as a function of shorter life spans (1 year lost out of 30 > 1 year lost out of 80), and might help them put some of the global health figures into a different perspective - namely, that the cost of infectious diseases can be much higher in countries with shorter life spans.
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_DALY(my_epi)
#'@export


calculate_DALY <- function(epidemic)
	{
	epidemic_history <- reconstruct_epidemic_history(epidemic)
	compartments <- epidemic_history$possible_compartments
	
	disabilities <- c('Dead','Quarantined','Isolated','Infectious Symptomatic','Exposed Symptomatic')
	
	dalys <- numeric()

	for (i in 1:epidemic$N_hosts)
		{
		dalys[i] <- sum(diff(epidemic_history$time_elapsed)[which(unlist(epidemic_history$outcome[i,]) %in% disabilities)-1])
		}
	names(dalys) <- names(epidemic$hosts)
	return(dalys)
	}

# A useful exercise would be to have each GMF time step set equal to one year, and assume that the first time step of the GMF exercise pertains to the student's current age. Then have each student calculate their individual DALY, with the life expectancy given for their country of origin and their sex. We can ask the student what the relationship would be between the day they died and their expected life span, and how that fits into the DALY calculation.
# One interesting exercise might be to normalize DALY by the total number of time steps used, and express it as a fraction. This can help students appreciate how disease burden can increase as a function of shorter life spans (1 year lost out of 30 > 1 year lost out of 80), and might help them put some of the global health figures into a different perspective - namely, that the cost of infectious diseases can be much higher in countries with shorter life spans.
