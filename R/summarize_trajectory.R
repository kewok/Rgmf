#'Reconstruct the dynamics of each compartment
#'
#'For a given Epidemic object, returns the number of individuals in each compartment over time. \cr
#'
#'@usage summarize_trajectory(epidemic)
#'@param epidemic An Epidemic object
#'@return A matrix whose [i,j]th entry represents the number of individuals in compartment i on time step j
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'epidemic_summary <- summarize_trajectory(my_epi)
#'
#'# note the compartments simulated are given as the names of the rows of this matrix:
#'row.names(epidemic_summary)
#'
#'@export

summarize_trajectory <- function(epidemic)
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
	return(outcomes_table)
	}
