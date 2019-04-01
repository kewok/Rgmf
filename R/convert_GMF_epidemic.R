#' Convert a GMF epidemic json file into a data frame that resembles the outbreakteachR dataframe.
#'
#' \code{convert_GMF_epidemic} reads data from GMF epidemic that was downloaded from the GMF web application and returns a dataframe that resembles the data frame generated when the xlsx file is initially read into the outbreak-dataset-read.R function in the outbreakteachR library. Note the remaining functionality of outbreakteachR will be available if an SIR epidemic was simulated without control. For epidemics violating this case, YMMV for using the remaining functionality of that pack.
#'
#' @param GMF_json_file Full file path to where the epidemic file downloaded from GMF is stored.
#'
#' @export
#'
#' @aliases convert_GMF_epidemic
#'

convert_GMF_epidemic <- function(epifile)
	{
	ntwk <- fromJSON(file=epifile)$epidemic_network
	shft <- fromJSON(file=epifile)$host_histories

	get_hosts_involved <- function(x)
		{
		if (is.null(x$source_host))
			{
			return(c(x$infected_host, NA))
			}
		else
			{
			return(c(x$infected_host, x$source_host))
			}
		}
	# The first column appears to be the host ID from each infection event and the second column the originating hosts. To identify the hosts involved, including the index case:
	infs <-  plyr::ldply(ntwk, get_hosts_involved)[,2:3]

	colnames(infs)[1:2] <- c('ID','Parent.ID')
	# The third column of the spread sheet is whether this is a reinfection event. To calculate this, determine the number of times the infected host appears, and declareall but their first appearence a reinfection event.

	reinfection <- function(x)
		{
		if (sum(infs[,1]==x) > 1)
			{
			return(c(0,rep(1,sum(infs[,1]==x)-1)))
			}
		else
			return(0)
		}


	infs <- cbind(infs,1:nrow(infs))

	for (i in unique(infs[,1]))
		{
		infs[which(infs[,1]==i),3] <- reinfection(i)
		}

	colnames(infs)[3] <- c('Reinfection')

	# The timing of the infection works as follows:
	start_time <- strptime(fromJSON(file=epifile)$epidemic_properties$epidemic_signup_deadline, "%Y-%m-%dT%H:%M:%S")

	get_infection_times <- function(x)
		{
		time_of_infection <- strptime(x$infection_date, "%Y-%m-%dT%H:%M:%S")
		return(c(as.character(as.Date(time_of_infection)), as.character(strftime(time_of_infection,format="%H:%M:%S")), as.character(as.numeric(difftime(time_of_infection,start_time,units='hours')))))
		}

	# Combine the starttime with the results of get_infection_times.
	begInf <- plyr::ldply(ntwk, get_infection_times)[,2:4]

	infs <- cbind(infs, begInf)
	colnames(infs)[4:6] <- c('Infection_Date','Infection_Time','Infection_Hours.since.start')

	# Identify the time at which symptoms begin by noting when the hosts become symptomatic
	symptoms_events <- function(x)
		{
		if ((x$destination_compartment == 'Is') || (x$destination_compartment == 'Es'))
			return(TRUE)
		else
			return(FALSE)
		}

	# Entries for which compartment transitions involve symptom onsets are:
	symptoms <- unique(which(sapply(shft,symptoms_events)))

	get_compartment_shift_times <- function(x)
		{
		time_of_onset <- strptime(x$switch_time, "%Y-%m-%dT%H:%M:%S")
		return(c(x$host, as.character(as.Date(time_of_onset)), as.character(strftime(time_of_onset,format="%H:%M:%S")), as.character(as.numeric(difftime(time_of_onset,start_time,units='hours')))))
		}

	symptom_onsets <- get_compartment_shift_times(shft[[symptoms[1]]])
	for (i in 2:length(symptoms))
		{
		symptom_onsets <- rbind(symptom_onsets, get_compartment_shift_times(shft[[symptoms[i]]]))
		}

	# Determine which infection events the symptom onsets are affiliated with and place to values in matrix infs
	colnames(symptom_onsets) <- c('ID','Date','Time','Hours since start')

	infs <- cbind(infs, rep(NA,nrow(infs)), rep(NA,nrow(infs)), rep(NA,nrow(infs)))
	colnames(infs)[7:9] <- c('Symptoms_Date','Symptoms_Time','Symptoms_Hours.since.start')
	for (i in 1:nrow(symptom_onsets))
		{
		same_host <- which(infs[,'ID']==symptom_onsets[i,'ID'])
		possible_infection_events <- which(infs[same_host,'Infection_Hours.since.start'] <= symptom_onsets[i,'Hours since start'])
		associated_infection <- max(possible_infection_events)  # identify the most recent infection event
		infs[same_host[associated_infection],7:9] <- symptom_onsets[i,2:4]
		}

	# Entries for which compartment transitions involve end of symptoms are:

	end_symptoms_events <- function(x)
		{
		if ( ((x$source_compartment == 'Is') || (x$source_compartment == 'Es')) && ((x$destination_compartment != 'Is') && (x$destination_compartment != 'Es')) )
			return(TRUE)
		else
			return(FALSE)
		}

	# Entries for which compartment transitions involve end of symptoms are:
	end_symptoms <- unique(which(sapply(shft,end_symptoms_events)))

	symptoms_ending <- get_compartment_shift_times(shft[[end_symptoms[1]]])
	for (i in 2:length(end_symptoms))
		{
		symptoms_ending <- rbind(symptoms_ending, get_compartment_shift_times(shft[[end_symptoms[i] ]]))
		}

	colnames(symptoms_ending) <- c('ID','Date','Time','Hours since start')

	# Determine which infection events the symptoms end and place those values in matrix infs

	infs <- cbind(infs, rep(NA,nrow(infs)), rep(NA,nrow(infs)), rep(NA,nrow(infs)))
	colnames(infs)[10:12] <- c('End_Infection_Date','End_Infection_Time','End_Infection_Hours.since.start')
	for (i in 1:nrow(symptoms_ending))
		{
		same_host <- which(infs[,'ID']==symptoms_ending[i,'ID'])
		associated_infection <- max(which(infs[same_host,'Infection_Hours.since.start'] <= symptoms_ending[i,'Hours since start']))  # identify the most recent infection event
		infs[same_host[associated_infection],10:12] <- symptoms_ending[i,2:4]
		}

	# Determine successful secondary infections resulting from each infection event


	get_secondary_infections <- function(x)
		{
		return(c(x$infections_attempted, x$infections_succeeded))
		}

	infs <- cbind(infs, plyr::ldply(ntwk, get_secondary_infections)[,2:3])
	colnames(infs)[13:14] <- c('Attempted', 'Successful') 

	# Determine time of first successful secondary infection; begin by identifying all infection events between when the source host got infected and potentially reinfected

	infs <- cbind(infs, rep(NA,nrow(infs)))

	colnames(infs)[15] <- c('Onward_Infection_Hours.since.start')
	for (host in unique(infs[,'Parent.ID']))
		{
		infections_to_host <- which(infs[,'ID']==host)
		infections_from_host <- which(infs[,'Parent.ID']==host)

		# If there are no reinfection events:
		if (sum(infs[infections_to_host,'Reinfection']==0))
			{
			infs[infections_to_host,'Onward_Infection_Hours.since.start'] = min(infs[infections_from_host,'Infection_Hours.since.start'])
			}
		# identify secondary infections that occurred between infection and reinfection events
		else
			{
			for (i in infections_from_host)
				{
				most_recent_infection <- max(which(infs[infections_to_host, 'Infection_Hours.since.start'] <= infs[i,'Infection_Hours.since.start']))
				# next_infection <- min(which(infs[infections_to_host,'Infection_Hours.since.start'] > infs[i,'Infection_Hours.since.start']))
				# If the timing of a secondary infection arising from this infection event is yet to be noted
				if (is.na(infs[infections_to_host[most_recent_infection],'Onward_Infection_Hours.since.start']))
					{
					infs[infections_to_host[most_recent_infection],'Onward_Infection_Hours.since.start'] <- infs[i,'Infection_Hours.since.start']
					}
				}
			}
		}
	return(as.data.frame(infs))
	}
