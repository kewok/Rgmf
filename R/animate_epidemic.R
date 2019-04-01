#'Animate epidemic
#'
#'For a given epidemic, loop through the visualizations of the epidemic over time to create an animation.\cr
#'
#'@usage animate_epidemic(epidemic, infections_only=F, save_as_gif = F, gif_name="GMF_Simulation.gif", animation_options=list("interval"=0.25, "loop"=1))
#'@param epidemic: An Epidemic object
#'infections_only: A logical value indicating whether only infection events should be displayed during the epidemic animation or if the entire epidemic simulation should be animated. Default is FALSE.
#'save_as_gif: This requires the animation package installed. A logical value indicating whether to save the animation as a GIF file. Default is FALSE.
#'gif_name: A character value specifying the file path to the GIF file that will be saved.
#'save_as_html: This requires the animation package installed. A logical value indicating whether to save the animation as an HTML file. Default is FALSE.
#'html_name: A character value specifying the file path to the HTML file that will be saved.
#'animation_options: A list of options to configure the saved animation in case a GIF or HTML file is to be created. See `ani.options' from the package `animation' for more details. The only three defaults set by `animate_epidemic' are the interval/time delay between images (defaults at 0.05 seconds or 0.25 seconds, respectively, for the entire epidemic or the case where only infection events are animated), the number of additional times the gif will loop through the epidemic (default at 1), and the "verbose" option for generating HTML animations (defaulse to FALSE).
#'@seealso 'animation'
#' `visualize_epidemic'
#'`graph' from the package `igraph'
#'`visualize_epidemic_at_infection_event
#'@examples
#'my_epi <- Epidemic('example_gmf.json')
#'
#'animate_epidemic(my_epi)
#'# Only show the last infection event, and save the outcome to a gif file.
#'visualize_epidemic(my_epi, infections_only=TRUE, save_as_gif=TRUE)
#'@export

animate_epidemic <- function(epidemic, infections_only=FALSE, save_as_gif=FALSE, gif_name="GMF_Simulation.gif", save_as_html=FALSE, html_name="GMF_Simulation.html", animation_options=list(interval=ifelse(infections_only, 0.25, 0.05), loop=1, verbose=FALSE))
	{
	dynamic_network <- contagion_network_evolution(epidemic)
	if (infections_only)
		{
		if (!(save_as_gif) && !(save_as_html))
			{
			for (i in 1:length(dynamic_network$epidemic_networks))
				{
				visualize_epidemic_at_infection_event(epidemic, i, last_infection_only=T)
				}
			}
		if (save_as_gif)
			{
			print("Preparing GIF animation. This may take some time")
			if(!requireNamespace("animation", quietly = TRUE))
				{
				stop("Package \'animation\' is needed to save the epidemic animation as a GIF. Please install it first", call.=FALSE)
				}
			if(requireNamespace("animation", quietly = TRUE))
				{
				animation::ani.options(animation_options)
				animation::saveGIF(
				for (i in 1:length(dynamic_network$epidemic_networks))
					visualize_epidemic_at_infection_event(epidemic, i, last_infection_only=T), movie.name=gif_name
				)
				}
			}
		if (save_as_html)
			{
			print("Preparing HTML animation. This may take some time")
			if(!requireNamespace("animation", quietly = TRUE))
				{
				stop("Package \'animation\' is needed to save the epidemic animation as a GIF. Please install it first", call.=FALSE)
				}
			if(requireNamespace("animation", quietly = TRUE))
				{
				animation::ani.options(animation_options)
				animation::saveHTML(
				for (i in 1:length(dynamic_network$epidemic_networks))
					{
					visualize_epidemic_at_infection_event(epidemic, i, last_infection_only=T)}, 
				htmlfile=html_name, autobrowse=FALSE,title=paste(epidemic$epidemic_name, "outbreak")
				)
				}
			}
		}
	else
		{
		if (!(save_as_gif) && !(save_as_html))
			{
			visualize_epidemic(epidemic, 1)
			for (i in 2:length(unique(unlist(lapply(epidemic$hosts,'[[','host_switch_time')))))
				visualize_epidemic(epidemic, i, last_infection_only=T)
			}
		if (save_as_gif)
			{
			if(!requireNamespace("animation", quietly = TRUE))
				{
				stop("Package \'animation\' is needed to save the epidemic animation as a GIF. Please install it first", call.=FALSE)
				}
			if (requireNamespace("animation", quietly = TRUE))
				{
				animation::ani.options(animation_options)
				animation::saveGIF(
						for (i in 1:epidemic$steps_iterated)
							visualize_epidemic(epidemic, i, last_infection_only=T), movie.name=gif_name
						)
				}
			}
		if (save_as_html)
			{
			if(!requireNamespace("animation", quietly = TRUE))
				{
				stop("Package \'animation\' is needed to save the epidemic animation as a GIF. Please install it first", call.=FALSE)
				}
			if(requireNamespace("animation", quietly = TRUE))
				{
				animation::ani.options(animation_options)
				animation::saveHTML(
				for (i in 1:length(dynamic_network$epidemic_networks))
					{
					visualize_epidemic_at_infection_event(epidemic, i, last_infection_only=T)}, 
				htmlfile=html_name,  autobrowse=FALSE,title=paste(epidemic$epidemic_name, "outbreak")
				)
				}
			}
		}
	}
