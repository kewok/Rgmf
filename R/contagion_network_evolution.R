#'Create a time series of contagion networks
#'
#'For a given epidemic, return an object of type contagion_network. Suppose there are n infection events over the course of the epidemic. Then the contagion_network object consists of two attributes: a list of n igraph instances describing current and past infection events, and a vector of when those infection events took place. \cr
#'
#'@usage contagion_network_evolution(epidemic)
#'@param epidemic: An Epidemic object
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE. 
#'@return sorted_infection_timings: a vector whose ith element describes the time in which the ith infection occurred.
#'epidemic_networks: a list whose ith element is an igraph instance whose edges represent an infection event that occurred either during the ith infection event or prior to it. The vertices of the ith element of the epidemic_networks list represent the hosts involved in a given infection event, with the first element of the vertex describing the source host and the second element representing the recipient host.
#'@seealso `graph' from the package `igraph'
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'dynamic_network <- contagion_network_evolution(my_epi)
#'
#' #examine the output:
#' dynamic_network
#'@export

contagion_network_evolution <- function(epidemic, use_symptomatic_only=FALSE)
	{
	contagion_graph <- reconstruct_epidemic_network(epidemic, use_symptomatic_only)
	infection_timings <- igraph::E(contagion_graph)$infection_time
	sorted_infection_timings <- unique(sort(unlist(infection_timings)))
	applicable_nodes <- numeric()
	contagion_network <- list()
	epidemic_networks <- list()
	contagion_network$epidemic_networks <- epidemic_networks
	for (i in 1:length(sorted_infection_timings))
		{
		applicable_nodes <- c(applicable_nodes, which(infection_timings==sorted_infection_timings[i]))
		contagion_network$epidemic_networks[[i]] <- igraph::subgraph.edges(contagion_graph, applicable_nodes)
		}
	contagion_network$sorted_infection_timings <- sorted_infection_timings
	class(contagion_network) <- append(class(contagion_network), 'Epidemic')
	return(contagion_network)
	}

