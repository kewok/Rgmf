#'The Epidemic object
#'
#'The function `Epidemic' takes as its argument the path to the json file produced by Give Me Fever! and creates an Epidemic object for use in R. This Epidemic object aims to reflect the contents of the json file describing the state of the Give Me Fever! epidemic itself at a given point in time. Put differently, an Epidemic object can be thought of as representing the Give Me Fever! save file, so that coordinators and instructors can represent the outcome of the epidemic using R. However, the correspondence between an Epidemic object from RGMF and the primary json file is not one-to-one; during construction, a few aspects of the json file are removed to facilitate its use in RGMF rather than the main app simulator. \cr
#'
#'@usage Epidemic(epidemic_file)
#'@param epidemic_file A character variable that specifies the path to the json for the epidemic state. 
#'@return An Epidemic object that represents the epidemic state in R. \cr The key attributes are: 
#'\itemize{
#'\item N_hosts: the number of hosts that participated in the epidemic
#'\item epidemic_parameters: a list consisting of the inputs that configured the epidemic for Give Me Fever!'s epidemic simulator. See details for an in-depth description of the named members
#'\item compartments: a list of the compartments which are simulated by the underlying agent-based epidemiological model. Each element of the list consists of the compartment as it was specified during configuration by the epidemic coordinator. See below for details.
#'\item epidemic_network: A structured list, each member of which represents a participant's effect on the overall epidemic. Each member of epidemic_network is itself a structured list. If a participant had been infected, its entry in epidemic_network consists of a list of each other parcipant whom they infected. The entries of this list consist of the name of the participant infected by the focal participant and the time step at which the infection took place.
#'\item hosts: a list describing the outcome of the epidemic for each participant. See details for further clarification.
#'\item patient_zero: a character variable specifying the name of the first participant to get infected with the disease
#'\item steps_iterated: a numeric variable specifying the number of steps iterated up to the imported epidemic state.
#'}
#'@section Attribute details:
#' Here we describe in greater detail three of the attributes above: the epidemic_parameters attribute, the hosts attribute and the compartments attribute. \cr
#
#'The epidemic_parameters attribute of an Epidemic object is a list consists of the various inputs that go into defining a Give Me Fever! epidemic. This list consists of the following named members\cr
#'\itemize{
#'\item compartment_model: a character value specifying the compartment model, 
#'\item compartments_simulated: a character vector specifying the compartments simulated, 
#'\item disease_name: a character value for the name of the disease, 
#'\item transition_rates: a structured list specifying, for each compartment simulated, the probabilities of transitioning into any of the possible destination compartments, 
#'\item transmission_constant: the baseline transmission constant (i.e., beta)
#'\item transmission_mode: a character value specifying whether infections are frequency- or density-dependent, and 
#'\item treatment_inputs: a structured list specifying the effectiveness or success probability of each available treatment.
#'}\cr
#'
#'The hosts attribute of an Epidemic object is a list consisting of descriptions of the participants over the course of the epidemic. Each participant is themself represented as a list, whose members consists of:\cr
#'\itemize{
#'\item host_history: a character vector whose ith element is the ith epidemic compartment through which a host passed
#'\item host_name: the host's name
#'\item host_switch_time: a numeric vector whose ith element is the time step on which a host switched from the ith compartment through which they passed
#'} \cr

#'The final attribute of the Epidemic class consists of the compartments list. Each member of the compartments list specifies details about the compartment simulated, and consists of the following members:\cr
#'\itemize{
#'\item compartment_name: a character value that names the compartment
#'\item members: a character vector of the names of the hosts/participants that are members of the compartment
#'\item number_of_possible_destinations: a numeric vector specifying the number of potential destinations from this compartment
#'\item transition_rates: a numeric list of the transition rates between this compartment and one of its destination compartments
#'}
#'@usage
#'my_epi <- Epidemic('example_gmf.json')
#'
#'#Note the number of individuals in each compartment over time can be visualized by calling plot() 
#'
#'plot(my_epi)
#'@export 

Epidemic <- function(epidemic_file)
	{
	epidemic_contents <- rjson::fromJSON(json_str = readChar(epidemic_file, file.info(epidemic_file)$size))

	epidemic_state <- list()

	# Apply some fixes to simplify the variables within R:
	epidemic_state$N_hosts <- length(epidemic_contents$epidemic_properties$hosts)
	epidemic_state$epidemic_name <- epidemic_contents$epidemic_properties$name
	epidemic_state$steps_iterated <- epidemic_contents$epidemic_properties$steps_iterated
	epidemic_state$hosts <- setup_hosts(epidemic_contents)
	epidemic_state$patient_zero <- epidemic_contents$host_histories[[1]]$host # id_patient_zero(epidemic_contents)
	epidemic_state$epidemic_parameters <- setup_parameters(epidemic_contents)
	epidemic_state$epidemic_network <- epidemic_contents$epidemic_network
	
	class(epidemic_state) <- append(class(epidemic_state), 'Epidemic')
	return(epidemic_state)
	}

#'Configure the hosts
#'
#'For a json string, return a list of hosts, which each element is itself a list.  \cr
#'
#'@usage Used internally by Epidemic()
#'@param Epidemic_JSON JSON string decribing the structure of the epidemic
#'@return A list of hosts, where each element is itself a list.
#'@seealso `Epidemic'
#'@export

setup_hosts <- function(Epidemic_JSON)
	{
	Compartment <- c("S"="Susceptible", "Ea"="Exposed Asymptomatic","Es"="Exposed Symptomatic", "Ia"="Infectious Asymptomatic", "Is"="Infectious Symptomatic", "R"="Recovered", "D"="Dead", "K"="Isolated", "Q"="Quarantined", "M"="Medicated","V"="Vaccinated")

	get_host <- function(x) return(x$host)
	changing_hosts <- lapply(Epidemic_JSON$host_histories, get_host)

	hosts <- list()
	host_names <- c()
	for (i in 1:length(Epidemic_JSON$epidemic_properties$hosts))
		{
		host <- list()
		host$host_name=Epidemic_JSON$epidemic_properties$hosts[i]
		host$host_history = c('Susceptible')
		names(host$host_history)[1] <- 'S'
		host$host_switch_time <- c()
		host_names[i] <- host$host_name

		host_changing_event <- which(changing_hosts==host$host_name)
		if (length(host_changing_event))
			{
			for (j in host_changing_event)
				{
				host$host_history <- c(host$host_history, Compartment[Epidemic_JSON$host_histories[[j]]$destination_compartment])
				host$host_switch_time <- c(host$host_switch_time, Epidemic_JSON$host_histories[[j]]$switch_time)
				}
			}
		hosts[[i]] = host
		}
	names(hosts) <- host_names
	return(hosts)
	}


#'Return patient zero
#'
#'From the list of hosts and their switch times, determine which host was patient zero \cr
#'
#'@usage Used internally by Epidemic()
#'@param Epidemic_JSON JSON string decribing the epidemic
#'@return The name of the host that was the index case
#'@seealso `Epidemic'
#'@export


id_patient_zero <- function(Epidemic_JSON)
	{
	get_min_switch_time <- function(x) return(min(x$switch_time))
	switch_times <- unlist(lapply(Epidemic_JSON$host_histories, get_min_switch_time))
	# if there is a tie? There shouldn't be on the microsecond scale, but...
	return(Epidemic_JSON$host_histories[[which(switch_times==min(switch_times))]]$host)
	}


#'Configure the epidemic parameter
#'
#'For a json string, return a list of epidemic parameter, which each element is itself a list.  \cr
#'
#'@usage Used internally by Epidemic()
#'@param Epidemic_JSON JSON string decribing the epidemic
#'@return A list of epidemic parameter, where each element is itself a list.
#'@seealso `Epidemic'
#'@export

setup_parameters <- function(Epidemic_JSON)
	{
	get_host <- function(x) return(x$host)
	changing_hosts <- lapply(Epidemic_JSON$host_histories, get_host)

	parameters <- list()
	parameters$compartment_model <- Epidemic_JSON$epidemic_properties$compartmental_model
	parameters$compartments_simulated <- c("S"="Susceptible", "Ea"="Exposed Asymptomatic","Es"="Exposed Symptomatic", "Ia"="Infectious Asymptomatic", "Is"="Infectious Symptomatic", "R"="Recovered", "D"="Dead", "K"="Isolated", "Q"="Quarantined", "M"="Medicated","V"="Vaccinated")[names(unlist(Epidemic_JSON$compartments))]
	parameters$disease_name <- Epidemic_JSON$epidemic_properties$name
	parameters$transmission_rate <- Epidemic_JSON$epidemic_properties$transmission_rate
	parameters$time_step <- Epidemic_JSON$epidemic_properties$epidemic_time_step
	
	parameters$transition_rates <- list()
	compartment_names <- c()
	for (i in 1:length(Epidemic_JSON$compartments))
		{
		if (Epidemic_JSON$compartments[[i]][[1]]!="") # if entry is non-empty
			{
			parameters$transition_rates[[i]] <- rjson::fromJSON(Epidemic_JSON$compartments[[i]][[1]])
			names(parameters$transition_rates[[i]]) <- parameters$compartments_simulated[names(rjson::fromJSON(Epidemic_JSON$compartments[[i]][[1]]))]
			}
		compartment_names <- c(compartment_names, parameters$compartments_simulated[names(Epidemic_JSON$compartments[[i]])])	
		}
	names(parameters$transition_rates) <- compartment_names
	parameters$transmission_mode <- Epidemic_JSON$epidemic_properties$transmission_mode
	if (parameters$transmission_mode == 'FD')
		{
		parameters$hosts_encountered <- Epidemic_JSON$epidemic_properties$hosts_encountered
		}
	parameters$treatment_inputs <- Epidemic_JSON$epidemic_properties$intervention_settings

	return(parameters)
	}

