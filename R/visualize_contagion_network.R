#'Visualize the contagion network
#'
#'For a given epidemic and time step, visualize the contagion network up to that time point. When there are a large number of individual infections that must be depicted, the color for the edges associated with each infection will differ slightly. However, the color legend only denotes approximate times at infections. \cr
#'
#'@usage plot_contagion_network(epidemic, iteration)
#'@param epidemic: An Epidemic object
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE.
#'iteration: the infection event up to which the infection events will be visualize
#'@seealso contagion_network_evolution
#'`graph' from the package `igraph'
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'# Visualize the infection events up to the last infection event
#'visualize_contagion_network(my_epi, iteration=90)
#'@export

visualize_contagion_network <- function(epidemic, iteration, use_symptomatic_only=FALSE)
	{
	dynamic_network <- contagion_network_evolution(epidemic, use_symptomatic_only)

	if (iteration < 1)
		{
		warning('No infection events have taken place. Only the first infection event will be displayed.')
		infection_index <- 1
		}

	else
		{
		if (iteration > length(dynamic_network$sorted_infection_timings))
			{
			warning('Iteration out of range. Only the contagion network associated with the last infection will be displayed.')
			}
		infection_index <- iteration
		}
	node_val <- character()
	for (j in 1:length(igraph::E(dynamic_network$epidemic_networks[[infection_index]])))
		{
		# obtain the second, 'destination' host
		node_val[j] <- strsplit(attributes(igraph::E(dynamic_network$epidemic_networks[[infection_index]]))$vnames[j],split="\\|")[[1]][2]
		}
	
	susceptible_until <- unlist(igraph::E(dynamic_network$epidemic_networks[[infection_index]])$infection_time)
	names(susceptible_until) <- node_val
	# map the time to infection to a particular iteration:
	map_to_iter <- function(x)
		{
		which(sort(unique(susceptible_until))==x)
		}
	infection_times_as_iterations <- sapply(susceptible_until, map_to_iter)
	legend_vals <- round(convert_times(epidemic,sort(unique(susceptible_until))[1:infection_index]),4)
	# If you have too many entries, agglomerate them into ten bins
	if (length(legend_vals) > 13)
		{
		legend_vals <- quantile(legend_vals, 1:13/13)
		}
	myGraph <- dynamic_network$epidemic_networks[[infection_index]]
	# If a node appears twice, include its infection times in subsequent appearances as a label to the 
	igraph::E(myGraph)$color <- fields::tim.colors(infection_index)[infection_times_as_iterations]
c("WHITE",rep(grey(0.75), length(node_val)-1))

	vertex_cols = rep(grey(0.75), length(igraph::V(myGraph)))
	vertex_cols[which(igraph::V(myGraph)$name == epidemic$patient_zero)] = "WHITE"

	plot(myGraph,vertex.color=vertex_cols,xlim=c(-0.95,1.4), edge.arrow.size=ifelse(length(legend_vals)<3, 1, 1/log(length(legend_vals),3)), main=paste("Time point", round(convert_times(epidemic,max(susceptible_until,na.rm=T)),4)), edge.width=2.25, vertex.label.cex=1.5, vertex.label.color = "BLACK")
	legend(1.15,0.5,round(legend_vals,3),col=fields::tim.colors(length(legend_vals)),pch=rep(16,length(legend_vals)),cex=1.5)
	if (length(susceptible_until) <= 13)
		{
		legend(1.2,0.675,rep(NA,length(legend_vals)),pch=rep(NA,length(legend_vals)),title="Time of\ninfection",bty="n",cex=1.5)
		}
	else
		{
		legend(1.2,0.75,rep(NA,length(legend_vals)),pch=rep(NA,length(legend_vals)),title="Approx.\n time of\ninfection",bty="n",cex=1.5)
		}
	}
