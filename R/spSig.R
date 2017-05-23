#' Randomize input network to calculate individual/species-level standardized effect
#' sizes
#'
#' Repeatedly randomize pairwise interactions and determine the number of intransitive
#' interactions each species is involved in.
#'
#' @param disp.input An edge-list-like data frame, formatted like the exInput data.
#' @param network.size The subnetwork size (dyad, triad, quartet, etc.) at which to
#' assess transitivity.
#' @param cutoff The proportion of interactions a species needs to have won in order to
#' be considered the winner.
#' @param conservative TRUE or FALSE. Whether a tied interaction is set to a
#' bidirectional edge or the edges are removed entirely.
#' @param write.wd The path to the working directory where results will be written.
#' @param iterations The number of times to randomize the pairwise interactions.
#' @param cores The number of cores to employ for parallel processing. Set to 'seq' to
#' run sequentially (i.e. not in parallel).
#'
#' @details Rather than combining this file with the boilDown() function, these are
#' kept separate. Doing this facilitates multiple runs with the same parameters.
#' Successive results can be combined with a simple cbind call, ALTHOUGH THE FIRST
#' SPECIES COLUMN SHOULD ONLY BE INCLUDED ONCE.
#'
#' @return A data frame with the first column summarizing the species/individual in
#' question, and subsequent columns detailing the per-iteration number of intransitive
#' relationships that species/individual was involved in.
#'
#' @export
#'
#' @references Miller, E. T., D. N. Bonter, C. Eldermire, B. G. Freeman, E. I. Greig,
#' L. J. Harmon, C. Lisle, and W. M. Hochachka. 2017. Fighting over food unites the
#' birds of North America in a continental dominance hierarchy.
#' biorxiv https://doi.org/10.1101/104133
#'
#' @examples
#' \dontrun{
#' #load in the example data
#' data(exInput)
#'
#' #before doing anything, remove any RDS files in the tempdir, because they can
#' #cause issues with this example
#' toDelete <- list.files(tempdir(), pattern=".RDS")
#' file.remove(paste(tempdir(), toDelete, sep="/"))
#'
#' #subsample the data down and run a few iterations over it to illustrate function
#' exInput<-exInput[1:100,]
#' exInput[,1] <- as.factor(as.character(exInput[,1]))
#' exInput[,2] <- as.factor(as.character(exInput[,2]))
#'
#' temp <- spSig(disp.input=exInput, network.size=3, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), iterations=3, cores=4)
#' }

spSig <- function(disp.input, network.size, cutoff, conservative,
	write.wd, iterations, cores)
{
	#force the disp.input to character 
	disp.input <- data.frame(apply(disp.input, 2, as.character), stringsAsFactors=FALSE)
	
	#define the species involved, sort into alphabetical order, create a dataframe
	sortedSpecies <- data.frame(species=sort(unique(c(disp.input$source,
		disp.input$target))), stringsAsFactors=FALSE)
	
	#go into a for loop where you randomize the direction of all reported interactions and
	#calculate the number of intransitive interactions each species was in
	for(i in 1:iterations)
	{
		#randomize the interactions
		fakeInput <- data.frame(t(apply(disp.input, 1, sample)))
		
		#give names to the fake input
		names(fakeInput) <- c("source", "target")
		
		#calculate which networks are intransitive. writes to file
		transitivityChecker(fakeInput, network.size=network.size,
			cutoff=cutoff, conservative=conservative, write.wd=write.wd, cores=cores)

		#run the transCruncher function over those files, pull just the intransitive col
		#from that element of transCruncher results
		transResults <- transCruncher(network.size=network.size,
			conservative=conservative, read.wd=write.wd)$intrans.results[[1]]$intransitive
		
		#cbind these results to sortedSpecies
		sortedSpecies <- cbind(sortedSpecies, transResults)
	}
	
	sortedSpecies
}
