#'Plot the prevalence over time
#'
#'For a given epidemic, plot either the number or proportion of individuals carrying the pathogen over time \cr
#'
#'@usage plot_prevalence(epidemic)
#'@param epidemic An Epidemic object
#'@param proportion A logical value indicating whether the output should be as a proportion (default=TRUE) or in terms of the absolute number of individuals (FALSE)
#'@param use_symptomatic_only A Boolean variable indicating whether only symptomatic infections should be considered; default is FALSE. 
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'plot_prevalence(my_epi)
#'@export

plot_prevalence <- function(epidemic, proportion=TRUE, use_symptomatic_only=FALSE)
	{
	history <- reconstruct_epidemic_history(epidemic)
	categories <- history$possible_compartments
	individual_histories <- history$outcome
	outcomes_table <- matrix(ncol=ncol(individual_histories), nrow=length(categories))
	for (i in 1:ncol(individual_histories))
		{
		outcomes_table[,i] <- table(factor(individual_histories[,i],levels=categories))
		}
	rownames(outcomes_table) = categories
	if (proportion==TRUE)
		{
		if (use_symptomatic_only)
			{
			plot(history$time_elapsed, (outcomes_table['Exposed Symptomatic',] + outcomes_table['Infectious Symptomatic',])/(epidemic$N_hosts-outcomes_table['Dead',]),t='l',xlab='Time',ylim=range(0,1), ylab="Fraction of Infected Individuals", main=paste('Prevalence for ', epidemic$epidemic_parameters$disease_name,'\n epidemic'))
			}
		else
			{
			plot(history$time_elapsed, (outcomes_table['Exposed Asymptomatic',] + outcomes_table['Exposed Symptomatic',] + outcomes_table['Infectious Symptomatic',] + outcomes_table['Infectious Asymptomatic',])/(epidemic$N_hosts-outcomes_table['Dead',]),t='l',xlab='Time',ylim=range(0,1), ylab="Fraction of Infected Individuals", main=paste('Prevalence for ', epidemic$epidemic_parameters$disease_name,'\n epidemic'))
			}
		}
	else
		{
		if (use_symptomatic_only)
			{
			plot(history$time_elapsed, (outcomes_table['Exposed Symptomatic',] + outcomes_table['Infectious Symptomatic',]),t='l',xlab='Time',ylim=range(outcomes_table), ylab="Number of Infected Individuals", main=paste('Prevalence for ', epidemic$epidemic_parameters$disease_name,'\n epidemic'))
			}
		else
			{
			plot(history$time_elapsed, (outcomes_table['Exposed Asymptomatic',] + outcomes_table['Exposed Symptomatic',] + outcomes_table['Infectious Symptomatic',] + outcomes_table['Infectious Asymptomatic',]),t='l',xlab='Time',ylim=range(outcomes_table), ylab="Number of Infected Individuals", main=paste('Prevalence for ', epidemic$epidemic_parameters$disease_name,'\n epidemic'))
			}
		}
	}
