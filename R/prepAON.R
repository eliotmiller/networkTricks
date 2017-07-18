#' Prep data for AON plot
#'
#' Prepare data for an attribute-ordered network plot.
#'
#' @param ranks Numeric vector with names matching those in edge.list. No check is
#' made to ensure these match 1:1, onus on user.
#' @param edge.list A data frame of observations, with the first column as the dominant
#' species/individual and the second column as the subordinate species/individual.
#'
#' @details User should format edge.list so it looks like the example data (exInput).
#'
#' @return A list object, prepped for use in plotting an attribute-ordered network.
#'
#' @export
#'
#' @references Hobson, E. A., D. J. John, T. L. Mcintosh, M. L. Avery, and T. F. Wright.
#' 2015. The effect of social context and social scale on the perception of
#' relationships in monk parakeets. Current Zoology 61:55-69.
#' 
#' @author Elizabeth A. Hobson, https://sites.google.com/site/hobsonresearch/
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
#'
#' prepped <- prepAON(results, exInput)

prepAON <- function(ranks, edge.list)
{
	#first create an attribute data frame with species name in first column, x-position
	#(all equal to 1) as second column, and rank as third column
	atts <- data.frame(ids=names(sort(ranks, decreasing=TRUE)), x=1,
		rank=length(ranks):1, stringsAsFactors=FALSE)
	
	#start to create the data frame called el
	el <- plyr::count(edge.list)

	#change names to be in keeping with her table, convert to characters
	names(el) <- c("id1", "id2", "wins")
	el[,1] <- as.character(el[,1])
	el[,2] <- as.character(el[,2])

	el$rank.diff <- NA

	#go into a for loop where you replace rank.diff[i] with the actual value
	for(i in 1:dim(el)[1])
	{
		winnerRank <- atts$rank[atts$ids==el$id1[i]]
		loserRank <- atts$rank[atts$ids==el$id2[i]]
		el$rank.diff[i] <- winnerRank-loserRank
	}

	#set as igraph object, vertices=atts is critical!
	prepped <- graph.data.frame(el, directed = TRUE, vertices = atts)

	results <- list("prepped"=prepped, "atts.df"=atts)
	results
}
