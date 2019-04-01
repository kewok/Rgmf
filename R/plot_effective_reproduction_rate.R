#'Plot the effective reproduction rate over time
#'
#'For a given epidemic, plot the effective reproduction rate inferred from infection events over time estimated using the method of Wallinga and Teunis (see package R0 for details). The shaded region represents the 95% CI of the estimated reproduction number. Note the abscissa will be marked at infection events at which the effective reproduction rate is inferred.\cr
#'
#'@usage plot_effective_reproduction_rate(epidemic)
#'@param epidemic An Epidemic object
#'@param trez The time resolution into which infection events will be sorted, as a fraction of the maximum time elapsed; default is 0.1 (i.e., in 10% time steps).
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE.
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'plot_effective_reproduction_rate(my_epi)
#'@export

plot_effective_reproduction_rate <- function(epidemic, trez=0.1, use_symptomatic_only=FALSE)
	{
	r_e <- estimate_Re(epidemic, trez, use_symptomatic_only)
	numeric_timings <- quantile(convert_times(epidemic, contagion_network_evolution(epidemic, use_symptomatic_only)$sorted_infection_timings), 1:(1/trez-1)/(1/trez-1)) # The -1 is needed to account for the fact that the infection of patient zero will not count in the estimation of the effective reproductive rate
	plot(numeric_timings, r_e$R, ylab="Estimated effective reproduction rate", xlab="Time (sec)",pch=NA,ylim=range(r_e$conf.int))
	polygon(x=c(numeric_timings, rev(numeric_timings)),y=c(r_e$conf.int[,1],rev(r_e$conf.int[,2])),border=0,col=grey(0.85))
	lines(numeric_timings, r_e$R)
	axis(1,at=numeric_timings,lab=round(timings_elapsed,2))

	# Draw a dashed plot at Re=1
	abline(h=1,lty=3)
	}

