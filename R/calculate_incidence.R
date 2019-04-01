#'Calculate the incidence over time
#'
#'For a given epidemic, calculate incidence (i.e., the number of new cases) over time \cr
#'
#'@usage plot_incidence(epidemic)
#'@param epidemic An Epidemic object
# Remove time resolution from initial commit
#'@param trez The time resolution into which infection events will be sorted, as a fraction of the maximum time elapsed; default is 0.1 (i.e., in 10% time steps). 
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE. 
#'@return incidences: a vector whose ith element describes the number of infections during time period i
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'calculate_incidence(my_epi, trez=0.2)
#'@export

calculate_incidence <- function(epidemic, trez=0.1, use_symptomatic_only=FALSE)
	{
	total_time <- max(reconstruct_epidemic_history(epidemic)$time_elapsed)
	if (use_symptomatic_only)
		{
		timings_elapsed <- convert_times(epidemic, unlist(igraph::E(reconstruct_epidemic_network(epidemic,use_symptomatic_only=TRUE))$infection_time))
		}
	else
		{
		timings_elapsed <- convert_infection_times(epidemic)
		}
	incidences <- c()
	incidences[1] <- length(which(timings_elapsed < total_time*trez))
	for (i in 2:(1/trez))
		{
		tmin <- (i-1)*trez*total_time
		tmax <- i*trez*total_time
		incidences[i] <- length(intersect(which(timings_elapsed > tmin),which(timings_elapsed < tmax)))
		}
	return(incidences)
	}

