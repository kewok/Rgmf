#'Reconstruct the history of the epidemic
#'
#'For a given Epidemic object, returns an object of the class epidemic_history that notes the compartments simulated, and the epidemic history of each participant. \cr
#'
#'@usage reconstruct_epidemic_history(epidemic)
#'@param epidemic An Epidemic object
#'@return \itemize{
#'\item possible_compartments: potential epidemic compartments for individuals
#'\item outcome: a data frame whose ith row represents the ith participant of the epidemic, and whose jth column is the epidemic states of all individuals on time step j. Thus, entry [i,j] of outcome represents the epidemic state of the ith participant at time step j.
#'}
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'epidemic_record <- reconstruct_epidemic_history(my_epi)
#'@export

reconstruct_epidemic_history <- function(epidemic)
	{
	outcome <- t(as.matrix(reconstruct_host_history(epidemic, names(epidemic$hosts)[1])))
	for (i in 2:epidemic$N_hosts)
		{
		outcome <- rbind(outcome, reconstruct_host_history(epidemic, names(epidemic$hosts)[i]))
		}
	rownames(outcome) <- names(epidemic$hosts)
	colnames(outcome) <- 1:ncol(outcome)
	outcome <- as.data.frame(outcome)
	possible_compartments <- epidemic$epidemic_parameters$compartments_simulated
	timings <- sort(unique(unlist(lapply(epidemic$hosts,'[[','host_switch_time'))))
	options(digits.secs=6)
	time_elapsed <- c()
	for (i in 1:length(timings))
		{
		time_elapsed[i] <- difftime(as.POSIXct(timings[i],format="%Y-%m-%dT%H:%M:%OS"), as.POSIXct(timings[1],format="%Y-%m-%dT%H:%M:%OS"))
		}
	time_elapsed <- sort(time_elapsed)
	epidemic_history <- list(possible_compartments = possible_compartments, outcome = outcome, time_elapsed=time_elapsed)
	class(epidemic_history) <- 'epidemic_history'
	return(epidemic_history)
	}
