#' A function for calculating the case fatality rate
#'For a given Epidemic object, determine the herd immunity threshold, i.e., the fraction of hosts that must be immunized in order to prevent an epidemic from spreading. \cr
#'@usage calculate_herd_immunity_threshold(epidemic, time_varying)
#'@param epidemic An Epidemic object
#'@param time_varying A logical value indicating whether the herd-immunity threshold should be time varying. Default is 'FALSE'. See below for details.
#'@return a numeric value representing the herd-immunity threshold.
#'@details The herd immunity threshold at the onset of the epidemic is modeled as 1-1/R0, where R0 is the basic reproduction rate of the analogous continuous time, deterministic model for the simulated epidemic. When `time_varying' is selected as `TRUE', for each subsequent time step, the herd immunity threshold is modeled as 1-1/Re, where Re is the effective reproduction rate of the analogous continuous time model.
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_herd_immunity_threshold(my_epi)
#'
#'# To evaluate the herd-immunity threshold over time:
#'calculate_herd_immunity_threshold(my_epi, time_varying=TRUE)
#'@export

calculate_herd_immunity_threshold <- function(epidemic, time_varying=FALSE)
	{
	if (time_varying)
		{
		return(1-1/(estimate_Re(epidemic)$R))	
		}
	else
		{
		return(1-1/(estimate_Re(epidemic)$R[1]))
		}
	}


