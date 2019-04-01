#'Plot the incidence over time
#'
#'For a given epidemic, plot incidence (i.e., the number of new cases) over time \cr
#'
#'@usage plot_incidence(epidemic)
#'@param epidemic An Epidemic object
#'@param trez The time resolution into which infection events will be sorted, as a fraction of the maximum time elapsed; default is 0.1 (i.e., in 10% time steps). The plot will display the incidences at the midpoint of the ith time interval.
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE. 
#'@examples
# Remove time resolution from initial commit
#'my_epi <- Epidemic('example_gmf.json')
#'
#'plot_incidence(my_epi, trez=0.2)
#'@export


plot_incidence <- function(epidemic, trez=0.1,use_symptomatic_only=FALSE)
	{
	incidences <- calculate_incidence(epidemic, trez, use_symptomatic_only)
	total_time <- max(reconstruct_epidemic_history(epidemic)$time_elapsed)
	options(digits.secs=6)
	timings_elapsed <- convert_infection_times(epidemic)
	time_points <- c()
	time_points[1] <- total_time * trez / 2
	for (i in 2:(1/trez))
		{
		tmin <- (i-1)*trez*total_time
		tmax <- i*trez*total_time
		time_points[i] <- (tmin + tmax)/2
		}
	plot(time_points, incidences,xlab="Time",ylab="Number of new cases",type="h")
	}

#'Plot the cumulative incidence over time
#'
#'For a given epidemic, plot cumulative incidence (i.e., the number of cases) over time \cr
#'
#'@usage plot_incidence(epidemic)
#'@param epidemic An Epidemic object
#'@param trez The time resolution into which infection events will be sorted, as a fraction of the maximum time elapsed; default is 0.1 (i.e., in 10% time steps). The plot will display the incidences at the midpoint of the ith time interval.
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'plot_incidence(my_epi)
#'@export

plot_cumulative_incidence <- function(epidemic,trez=0.1)
	{
	incidences <- calculate_incidence(epidemic, trez)
	total_time <- max(reconstruct_epidemic_history(epidemic)$time_elapsed)
	# Needing to recalculating timings_elapsed is a violation of DRY:
	options(digits.secs=6)
	timings_elapsed <- convert_infection_times(epidemic)
	time_points <- c()
	time_points[1] <- total_time * trez / 2
	for (i in 2:(1/trez))
		{
		tmin <- (i-1)*trez*total_time
		tmax <- i*trez*total_time
		time_points[i] <- (tmin + tmax)/2
		}
	plot(time_points, cumsum(incidences),t="l",xlab="Time",ylab="Cumulative incidence")
	}
