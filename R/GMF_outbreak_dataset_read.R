#' Read in and remove sensitive data
#'
#' \code{outbreak_dataset_read} reads data from GMF epidemic that was downloaded from the GMF web application and prepares it for use with outbreakteachR.
#'
#' @param GMF_json_file Full file path to where the epidemic file downloaded from GMF is stored.
#' @param attempt.imputation Boolean detailing whether to impute data. If TRUE (default),
#' imputation will occur but if FALSE then it will not automatically happen
#' @param fill.in.end.infection.hours Boolean detailing whether to fill in empty end infection hours. Will be needed
#' if \code{outbreak_dataset_read} throws an error associated with missing data.
#'
#' @export
#'
#' @aliases outbreak_dataset_read
#'

outbreak_dataset_read <- function(GMF_json_file, attempt.imputation=TRUE, fill.in.end.infection.hours=FALSE){

  df <- convert_GMF_epidemic(GMF_json_file)

  # grab seed positions
  seeds <- which(is.na(df$Parent.ID))

  # turn into useful numeric type; since GMF uses host names, if needed these are converted to numerical values at a later step
  df[,c(6,9,12,13,14,15)] <- suppressWarnings(lapply(df[,c(6,9,12,13,14,15)],as.numeric))

  host_names <- unique(c(df[,1],df[!is.na(df[,2]),2]))
  map_numeric <- data.frame(host_names, 1:length(host_names),stringsAsFactors=FALSE) # in the absence of hash tables
  convert_to_numeric <- function(x)
	{
	return(as.numeric(map_numeric[which(map_numeric[,1]==x),2]))
	}
  df[,1] <- sapply(df[,1],convert_to_numeric)
  df[!is.na(df[,2]),2] <- sapply(df[!is.na(df[,2]),2],convert_to_numeric)
  df[,2] <- as.numeric(df[,2])

  # tidy logicals
  df$Reinfection <- as.logical(df$Reinfection)

  ## ERROR CHECKING ##
  ## --------------------------------------------------------------------------

  # If we have decided to error check
  if(attempt.imputation==TRUE){

    # first work out the non reinfections
    non.reinfections <- which(df$Reinfection==FALSE)

    ## ERROR CHECK 1: Looking for empty end infections and missing parents
    ## --------------------------------------------------------------------------

    # find missing end infections and parents and then remove the seeds
    wrong_rows <- non.reinfections[which(is.na(df$End_Infection_Hours.since.start[non.reinfections]) | is.na(df$Parent.ID[non.reinfections]))]
    wrong_rows <- wrong_rows[-match(seeds,wrong_rows)]

    # if we are missing some end infections
    if(length(wrong_rows)>0){

      error.message <- paste("End Infection Hours since start column missing data at rows:\n",paste(wrong_rows+2,collapse=", "),
                             "\n Attempting Data Imputation...")
        ## Imputation

        for(i in wrong_rows){

          ## First, does the missing data not have a parent, otherwise we work out the most likely parent
          if(is.na(df$Parent.ID[i])){

          ## first what's the beinning infection hour and if it's not there then quit
          begin.inf <- df$Infection_Hours.since.start[i]
          if(!is.numeric(begin.inf)) stop("Data can't be imputed for end of infection if beginning is not known")

          # which individuals had infection times before this and hadn't already ended their infection
          possible.rows <- intersect(which(df$End_Infection_Hours.since.start > begin.inf),
                                     which(df$Infection_Hours.since.start < begin.inf))

          # then let's see of these how many did they infect and probabilistically given the poisson distribution who is most
          # likely to have infected another individual

          # recorded attempted, which will use for the dpois bounds and then find out which attempted infections
          # for the possible rows is most likly to have occured if it was +1
          attempted.infections <- df$Attempted.[possible.rows]
          att.inf.range <- sort(unique(attempted.infections))
          sorted.probs <- sort.int((dpois(att.inf.range + 1,1.8)),decreasing = TRUE,index.return = T)$ix

          # Which possible rows have the greatest chance of having another infection
          more.possible.rows <- possible.rows[which(attempted.infections==(att.inf.range[sorted.probs[1]]))]

          # If more than one row is still here then sample
          if(length(more.possible.rows)>1){
            imputed.parent.row <- sample(more.possible.rows,size = 1)
          } else {
            imputed.parent.row <- more.possible.rows
          }

          # Fill in the blank parent row
          df$Parent.ID[i] <- df$ID[imputed.parent.row]

          # if the newly chosen parent did not have an onward infection then add the onward infection time
          if(is.na(df$Onward_Infection_Hours.since.start[imputed.parent.row])){
            df$Onward_Infection_Hours.since.start[imputed.parent.row] <- df$Infection_Hours.since.start[i]
          }

          # If the parent was known just found the row for later
          } else {
            imputed.parent.row <- which(df$ID == df$Parent.ID[i])
          }

          # Second, if they don't have and end infection then pick a likely time
          # between the individuals infection begin and end infection and round to 2dp
          if(is.na(df$End_Infection_Hours.since.start[i])){

          # if this indvidual caused secondary infections then they must have ended after this time
          children <- which(df$Parent.ID == df$ID[i])
          if(length(children > 1)){
            min.start <- max(df$Infection_Hours.since.start[children],na.rm = T)
          } else {
            min.start <- df$Infection_Hours.since.start[i]
          }
          df$End_Infection_Hours.since.start[i] <- round(runif(1,min=min.start,
                                                               max=min( df$Infection_Hours.since.start[i]+48,
                                                                        max(df$End_Infection_Hours.since.start,na.rm=T))
                                                               ), digits = 2)
          }

        }

    }

    ## ERROR CHECK 2: Looking for empty begin infections
    ## --------------------------------------------------------------------------
    wrong_rows <- non.reinfections[which(is.na(df$Infection_Hours.since.start[non.reinfections]))]
    if(length(wrong_rows)>0) stop (paste("Begin Infection Hours since start column missing data at rows",paste(wrong_rows+2,collapse=", ")))

    ## ERROR CHECK 3: Looking for missing onward infection times given known parent ids for these individuals exist
    ## --------------------------------------------------------------------------
    wrong_rows <- non.reinfections[which(is.na(df$Onward_Infection_Hours.since.start[match(df$Parent.ID[non.reinfections][!is.na(df$Parent.ID[non.reinfections])],df$ID)]))]
    if(length(wrong_rows)>0) stop (paste("Missing time of onward infection information for individuals who cause non-reinfection infections at rows",paste(wrong_rows+2,collapse=", ")))

    # If we have decided not to error check then simply fill the end infection hours in
  } else if (fill.in.end.infection.dates == TRUE) {
    non.reinfections <- which(df$Reinfection==FALSE)
    wrong_rows <- non.reinfections[which(is.na(df$End_Infection_Hours.since.start[non.reinfections]))]
    df$End_Infection_Hours.since.start[wrong_rows] <- max(df$End_Infection_Hours.since.start,na.rm=TRUE)

    ## Something to do with parents
    ## TODO:
  }

  df$Infection_Date <- strftime(df$Infection_Date,"%Y/%m/%d")
  df$Symptoms_Date <- strftime(df$Symptoms_Date,"%Y/%m/%d")
  df$End_Infection_Date <- strftime(df$End_Infection_Date,"%Y/%m/%d")

  # Calculate epidemiological parameters
  df$Latent_Period_Hours <- df$Onward_Infection_Hours.since.start - df$Infection_Hours.since.start
  df$Incubation_Period_Hours <- df$Symptoms_Hours.since.start - df$Infection_Hours.since.start
  df$Infectious_Period_Hours <- df$End_Infection_Hours.since.start - df$Onward_Infection_Hours.since.start
  df$Generation_Time_Hours <- df$Infection_Hours.since.start - df$Infection_Hours.since.start[match(df$Parent.ID,df$ID,incomparables = NA)]

  # Remove errors in Generation Times
  error.pos <- which(df$Generation_Time_Hours<0)
  if(length(error.pos)>0){

    catch = 1

    df$Generation_Time_Hours[df$Generation_Time_Hours<0] <- NA
    message(paste("Warning: Some negative generation times were recorded and subsequently",
              "removed. Please check rows: \n ",paste(error.pos+2,collapse = ", ")))

  }

  # Format data to include all necessary contacts, i.e. fill in id number in columns where omitted
  counter <- 0
  for ( i in 1:length(df$ID)){

    if(!is.na(df$ID[i])){
      counter <- counter + 1
    } else {
      df$ID[i] <- counter
    }

  }

  ## Let's change those who were unifected to have this column equal to false for the sake of including them
  ## within graph plotting
  df$Reinfection[is.na(df$Reinfection)] <- FALSE

  ## remove unnecessary data if not required
  res <- df[,-c(13,14)]

  return(res)

}
