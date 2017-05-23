#' Derive Bradley-Terry coefficients
#'
#' Calculate the modified form of Bradley-Terry coefficients for all species in an
#' input edge list.
#'
#' @param edge.list A data frame of observations, with the first column as the dominant
#' species/individual and the second column as the subordinate species/individual.
#' @param cores The number of cores to use for parallel processing. Specify 'seq'
#' to run sequentially.
#'
#' @details User should format edge.list so it looks like the example data (exInput).
#' The returned object can have odd names. This can be fixed using agrep, per the
#' example.
#'
#' @return Named vector of Bradley-Terry coefficients for all species in edge.list.
#'
#' @export
#'
#' @importFrom BradleyTerry2 countsToBinomial BTm
#' @importFrom igraph graph.data.frame get.adjacency
#' @importFrom plyr rbind.fill
#'
#' @references Miller, E. T., D. N. Bonter, C. Eldermire, B. G. Freeman, E. I. Greig,
#' L. J. Harmon, C. Lisle, and W. M. Hochachka. 2017. Fighting over food unites the
#' birds of North America in a continental dominance hierarchy.
#' biorxiv https://doi.org/10.1101/104133
#'
#' @examples
#' #load in the example data
#' data(exInput)
#'
#' #run the Bradley Terry function. takes a few seconds to run.
#' results <- myBTM(edge.list=exInput, cores=2)
#'
#' #the names of these scores have weird periods in them. use agrep to solve
#' species <- unique(c(as.character(exInput$source), as.character(exInput$target)))
#' names(results) <- sub("..", "", x=names(results))
#'
#' for(i in 1:length(results))
#' {
#'   names(results)[i] <- species[agrep(names(results)[i], species, max.distance=0.1)]
#' }

myBTM <- function(edge.list, cores)
{
	doParallel::registerDoParallel(cores)

	#i think doing this will avoid R CMD check errors
	win1 <- NULL
	win2 <- NULL
	player1 <- NULL
	player2 <- NULL

	#convert the edge.list into a network graph
	tempGraph <- igraph::graph.data.frame(edge.list)

	#convert that into the correct format for BradleyTerry model
	forBTM <- BradleyTerry2::countsToBinomial(igraph::get.adjacency(tempGraph,
		sparse=FALSE))

	species <- unique(c(as.character(forBTM$player1), as.character(forBTM$player2)))

	if(cores != 'seq')
	{
		tempResults <- foreach(i = 1:length(species)) %dopar%
		{
			temp <- suppressWarnings(BradleyTerry2::BTm(cbind(win1, win2), player1, player2,
				data = forBTM, refcat=species[i]))
			#pull just the coefficients out of the model and force into a dataframe
			data.frame(t(data.frame(coeff=temp$coefficients)))
		}
	}
	else if(cores == 'seq')
	{
		tempResults <- list()

		for(i in 1:length(species))
		{
			temp <- suppressWarnings(BradleyTerry2::BTm(cbind(win1, win2), player1, player2,
				data = forBTM, refcat=species[i]))
			#pull just the coefficients out of the model and force into a dataframe
			tempResults[[i]] <- data.frame(t(data.frame(coeff=temp$coefficients)))
		}
	}

	else
	{
		stop("cores must be set to a number or 'seq'")
	}

	bound <- Reduce(plyr::rbind.fill, tempResults)

	BTscores <- apply(bound, 2, median, na.rm=TRUE)

	BTscores
}
