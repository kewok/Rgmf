default_color_scheme <- c("Susceptible"="white", "Exposed Symptomatic"="yellow", "Exposed Asymptomatic"="blue", "Infectious Symptomatic"="magenta","Infectious Asymptomatic"="red", "Recovered"="green", "Dead"="grey", "Isolated"="lightskyblue3", "Quarantined"="orange", "Medicated"="purple","Vaccinated"="pink")

#'Visualize epidemic
#'
#'For a given epidemic and time step, visualize the state of the epidemic at that time step.\cr
#'
#'@usage visualize_epidemic(epidemic, iteration, last_infection_only=F)
#'@param epidemic: An Epidemic object
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE.
#'@param iteration: the iteration up to which the epidemic will be visualized. If this value is greater than the number of steps iterated, only the final epidemic state will be illustrated
#'@param last_infection_only: a logical value indicating whether only the last infection event prior to the specified illustration should be illustrated with an arrow
#'@seealso visualize_contagion_network
#'`graph' from the package `igraph'
#'`visualize_epidemic_at_infection_event
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'# Visualize the epidemic state on the 50th iteration
#'visualize_epidemic(my_epi, iteration=50)
#'# Only show the last infection event:
#'visualize_epidemic(my_epi, iteration=50, last_infection_only=TRUE)
#'@export

visualize_epidemic <- function(epidemic, iteration, last_infection_only=F, use_symptomatic_only=FALSE)
	{
	dynamic_network <- contagion_network_evolution(epidemic, use_symptomatic_only)
	timings <- sort(unique(unlist(lapply(epidemic$hosts,'[[','host_switch_time'))))
	if (iteration > length(timings))
		{
		warning('Fewer time steps than requested were simulated. Showing the last simulated time step.')
		iteration <- length(timings)-1
		}
	hosts <- names(epidemic$hosts)
	
	grand_network <- igraph::graph_from_data_frame(t(combn(hosts,2)), directed=F, vertices=names(epidemic$hosts))
	# For the iteration, create a sub-network containing the nodes involved at that time step as follows:
	last_infection_event <- max(which(dynamic_network$sorted_infection_timings < timings[iteration]),0)
	"%s%" <- igraph::"%s%"
	final_network <- igraph::intersection(igraph::as.directed(grand_network) %s% dynamic_network$epidemic_networks[[length(dynamic_network$epidemic_networks)]])

	# Find the layout:
	layout_f <- igraph::layout.kamada.kawai(final_network)

	# Determine what the epidemic network looks like on step 'iteration':
	if (last_infection_event) #if at least one infection event has occurred
		{
		subnetwork <- igraph::intersection(igraph::as.directed(grand_network) %s%dynamic_network$epidemic_networks[[last_infection_event]])

		if (1 > iteration)
			{
			subnetwork <- igraph::delete_edges(subnetwork, igraph::E(subnetwork))
			}
		if (last_infection_only)
			{
			if (last_infection_event > 1) # provided there have been subsequent infections since the first
				{
				subnetwork <- subnetwork - dynamic_network$epidemic_networks[[last_infection_event-1]]
				}
			}
		}
	else
		{
		subnetwork <- igraph::delete_edges(grand_network,igraph::E(grand_network))
		}

	# Figure out the edges involved in the epidemic network thusfar
	
	# color code the host according to infection state:
	igraph::V(subnetwork)$color = "WHITE"
	# color code all hosts:
	for (host in names(igraph::V(subnetwork)))
		{
		epidemic_history <- reconstruct_host_history(epidemic, host)
		current_host_state <- epidemic_history[iteration]
		host_compartment_index <- which(names(epidemic$epidemic_parameters$compartments_simulated)==current_host_state)
		igraph::V(subnetwork)$color[which(names(igraph::V(subnetwork))==host)] <- default_color_scheme[current_host_state]
		}

	plot(subnetwork, layout=layout_f, edge.arrow.size=0.5, main=paste("Time step" , timings[iteration]))#ifelse(length(legend_vals)<3, 1, 1/log(length(legend_vals),3)))
	
	compartments <- epidemic$epidemic_parameters$compartments_simulated

	if (length(compartments) > 9)
		{
		addComp <- compartments[10:length(compartments)]
		compartments <- compartments[1:9]
		legend(-1.67,-1.2, ncol=4, names(default_color_scheme[compartments]),fill=default_color_scheme[compartments],box.lwd=0, pt.cex=1,cex=ifelse(length(compartments) > 5, 0.90, 1))
		legend(0.94,-1.2, names(default_color_scheme[addComp]), fill=default_color_scheme[addComp], box.lwd=0, pt.cex=1,cex=ifelse(length(compartments) > 5, 0.90, 1))
		}
	}


#'Visualize epidemic, indexed by infection events
#'
#'For a given epidemic and index of an infection event, visualize the state of the epidemic at that infection event.\cr
#'
#'@usage visualize_epidemic(epidemic, infection_index, last_infection_only=F)
#'@param epidemic: An Epidemic object
#'@param infection_index: the index of the infection event at which the epidemic will be visualized. If this value is greater than the number of infection events, only the final infection event will be illustrated. 
#'@param last_infection_only: a logical value indicating whether only the infection event at the indicated index should be illustrated with an arrow
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE.
#'@seealso visualize_contagion_network
#'`graph' from the package `igraph'
#'`visualize_epidemic_at_infection_event
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'# Visualize the epidemic state on the 4th infection event
#'visualize_epidemic_at_infection_event(my_epi, infection_index=4)
#'@export

visualize_epidemic_at_infection_event <- function(epidemic, infection_index, last_infection_only=F, use_symptomatic_only=FALSE)
	{
	timings <- sort(unique(unlist(lapply(epidemic$epidemic_network,'[[','infection_date'))))
	if (infection_index > length(timings))
		{
		warning('Fewer infections than requested were simulated. Showing the last infection.')
		infection_index <- length(timings)
		}
	hosts <- names(epidemic$hosts)
	
	grand_network <- igraph::graph_from_data_frame(t(combn(hosts,2)), directed=F, vertices=names(epidemic$hosts))
	# For the iteration, create a sub-network containing the nodes involved at that time step as follows:
	dynamic_network <- contagion_network_evolution(epidemic, use_symptomatic_only)

	"%s%" <- igraph::"%s%"
	final_network <- dynamic_network$epidemic_networks[[length(dynamic_network$epidemic_networks)]]

	# Find the layout:
	layout_f <- igraph::layout.kamada.kawai(final_network)

	# Determine what the epidemic network looks like on step 'iteration':
	last_network <- dynamic_network$epidemic_networks[[infection_index]]

	if (last_infection_only && (infection_index>1))
		{
		last_network <- last_network - dynamic_network$epidemic_networks[[infection_index-1]]	
		}
	# Figure out the edges involved in the epidemic network thusfar
	"%s%" <- igraph::"%s%"
	subnetwork <- igraph::intersection(igraph::as.directed(grand_network) %s% last_network)
	
	if (1 > infection_index)
		subnetwork <- igraph::delete_edges(subnetwork, igraph::E(subnetwork))

	# color code the host according to infection state:
	igraph::V(subnetwork)$color = "WHITE"
	# color code all hosts:
	for (host in names(igraph::V(subnetwork)))
		{
		epidemic_history <- reconstruct_host_history(epidemic, host)
		if (infection_index > 1)
			{
			iteration <- max(which(sort(unique(unlist(lapply(epidemic$hosts,'[[','host_switch_time')))) < timings[infection_index]))
			}
		else
			{
			iteration <- 1
			}
		current_host_state <- epidemic_history[iteration]
		host_compartment_index <- which(names(epidemic$epidemic_parameters$compartments_simulated)==current_host_state)
		igraph::V(subnetwork)$color[which(names(igraph::V(subnetwork))==host)] <- default_color_scheme[current_host_state]
		}

	plot(subnetwork, layout=layout_f, edge.arrow.size=0.5, main=paste("Infection number" , infection_index))#ifelse(length(legend_vals)<3, 1, 1/log(length(legend_vals),3)))

	compartments <- epidemic$epidemic_parameters$compartments_simulated


	if (length(compartments) > 9)
		{
		addComp <- compartments[10:length(compartments)]
		compartments <- compartments[1:9]
		legend(-1.67,-1.2, ncol=4, names(default_color_scheme[compartments]), fill=default_color_scheme[compartments],box.lwd=0, pt.cex=1,cex=ifelse(length(compartments) > 5, 0.90, 1))
		legend(0.94,-1.2, names(default_color_scheme[addComp]), fill=default_color_scheme[addComp], box.lwd=0, pt.cex=1,cex=ifelse(length(compartments) > 5, 0.90, 1))
		}
	}

