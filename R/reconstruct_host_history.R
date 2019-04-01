#'Reconstruct a particular host's epidemic history
#'
#'For a given Epidemic object and a participant's host name, return a character vector whose ith element is the host's epidemic state on time step i. \cr
#'
#'@usage reconstruct_host_history(epidemic, host_name)
#'@param epidemic: An Epidemic object
#'host_name: a character valued name of the host used by a participant
#'@return a character vector whose ith element is the host's epidemic state on time step i. 
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'host_x_epidemic_history <- reconstruct_host_history(my_epi, 'x')
#'@export

reconstruct_host_history <- function(epidemic, host_name)
	{
	times <- sort(unique(unlist(lapply(epidemic$hosts,'[[','host_switch_time'))))
	
	host_history <- c()
	if (host_name != epidemic$patient_zero)
		{
		host_history[1:which(times==epidemic$hosts[[host_name]]$host_switch_time[1])] <- rep("Susceptible", which(times==epidemic$hosts[[host_name]]$host_switch_time[1]))
		names(host_history)[1:which(times==epidemic$hosts[[host_name]]$host_switch_time[1])] <- rep("S", which(times==epidemic$hosts[[host_name]]$host_switch_time[1]))
		}
	else
		{
		first_non_susceptible_compartment <- min(which(epidemic$hosts[epidemic$patient_zero][[1]]$host_history!='Susceptible'))
		host_history[1:which(times==epidemic$hosts[[host_name]]$host_switch_time[1])] <- rep(epidemic$hosts[epidemic$patient_zero][[1]]$host_history[first_non_susceptible_compartment], which(times==epidemic$hosts[[host_name]]$host_switch_time[1]))
		names(host_history)[host_history[1:which(times==epidemic$hosts[[host_name]]$host_switch_time[1])]] <- 'S'
		}
	for (i in 2:length(epidemic$hosts[[host_name]]$host_switch_time))
		{
		next_step <- which(times==epidemic$hosts[[host_name]]$host_switch_time[i])
		host_history <- c(host_history, rep(epidemic$hosts[[host_name]]$host_history[i], next_step - which(times==epidemic$hosts[[host_name]]$host_switch_time[i-1])))
		}
	final_state <- epidemic$hosts[[host_name]]$host_history[length(epidemic$hosts[[host_name]]$host_history)]
	host_history <- c(host_history, rep(final_state, length(times)-next_step))
	return(host_history)
	}
