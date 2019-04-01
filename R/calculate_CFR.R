#' A function for calculating the case fatality rate
#'For a given Epidemic object, determine the case fatality rate over the course of the entire epidemic \cr
#'@usage calculate_CFR(epidemic)
#'@param epidemic An Epidemic object
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE. Setting this to true can affect the calculation of the generation time.
#'@return a numeric value representing the case fatality rate.
#'@details The case fatality rate is defined as the proportion of infections that result in death of the host during the course of an epidemic.
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_CFR(my_epi)
#'@export


calculate_CFR <- function(epidemic, use_symptomatic_only=FALSE)
	{
	epidemic_history <- reconstruct_epidemic_history(epidemic)$outcome
	total_number_of_cases <- sum(calculate_incidence(epidemic,use_symptomatic_only))
	final_state <- epidemic_history[,ncol(epidemic_history)]
	CFR <-  ifelse(!is.na(table(final_state)['Dead']), table(final_state)['Dead']/(sum(total_number_of_cases)), 0) 
	return(CFR)
	}
