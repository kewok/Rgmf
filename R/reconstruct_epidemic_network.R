reconstruct_epidemic_network <- function(epidemic, use_symptomatic_only=FALSE)
	{
	# omit the index case
	epidemic_network <- epidemic$epidemic_network
	epidemic_network[[1]] <- NULL
	source_host <- as.character(lapply(epidemic_network, '[[', 'source_host'))
	infected_host <- as.character(lapply(epidemic_network, '[[', 'infected_host'))
	infection_time <- lapply(epidemic_network, '[[', 'infection_date')
	contagion_network <- as.data.frame(cbind(source_host, infected_host, infection_time))
			
	if (use_symptomatic_only)
		{
		new_contagion_network <- as.data.frame(matrix(rep(NA,3),ncol=3))
		times <- sort(unique(unlist(lapply(epidemic$hosts,'[[','host_switch_time'))))
		for (host in unique(source_host))
			{
			ineligible_entries <- c()
			contagion_sub_network <- contagion_network[which(infected_host==host),]
			if (host==epidemic$patient_zero)
				{
				contagion_sub_network <- rbind(c('SEED',host,min(epidemic$host[host][[1]]$host_switch_time)), contagion_sub_network)
				}
			host_states <- reconstruct_host_history(epidemic, host)
			for (i in 1:nrow(contagion_sub_network))
				{
				if (i < nrow(contagion_sub_network))
					{
					relevant_time_points <- intersect(which(times < contagion_sub_network[i+1,'infection_time']), which(times >= contagion_sub_network[i,'infection_time']))
					}
				else
					{
					relevant_time_points <- which(times >  contagion_sub_network[i,'infection_time'])
					}
				if ('Infectious Asymptomatic' %in% host_states[relevant_time_points])
					{
					ineligible_entries <- c(i, ineligible_entries)
					}
				}
			if (length(ineligible_entries))
				{
				contagion_sub_network <- contagion_sub_network[-ineligible_entries,]
				}
			if (nrow(contagion_sub_network))
				{
				names(new_contagion_network) <- names(contagion_sub_network)
				new_contagion_network <- rbind(new_contagion_network, contagion_sub_network)
				}
			}
		contagion_network <- new_contagion_network[-1,]
		contagion_network <- contagion_network[-which(contagion_network[,'source_host']=='SEED'),]
		}
	contagion_network <- contagion_network[order(unlist(contagion_network[,'infection_time'])),]
	contagion_graph <- igraph::graph_from_data_frame(contagion_network, directed=TRUE, vertices=names(epidemic$hosts))
	return(contagion_graph)
	}
