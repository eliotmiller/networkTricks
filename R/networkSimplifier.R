#' Simplify an input network
#'
#' Threshold and simplify an input network.
#'
#' @param sub.graph An igraph-formatted network.
#' @param cutoff The proportion of interactions a species needs to have won in order to
#' be considered the winner.
#' @param conservative TRUE or FALSE. Whether a tied interaction is set to a
#' bidirectional edge or the edges are removed entirely.
#'
#' @details Takes a network and simplifies it according to a specified cutoff/threshold.
#' Specifically, it checks for parallel edges between each species combination. If they
#' exist, it calculates what proportion of interactions are A->B, and what proportion are
#' B->A. If either of these proportions are at or above the cutoff, it sets that edge
#' to 1, and the other edge to 0. If neither proportion is above the threshold, then it
#' either sets both edges to 0, or both 1, depending on whether conservative is set to
#' TRUE or FALSE.
#'
#' @return A simplified/thresholded igraph network.
#'
#' @export
#'
#' @importFrom igraph get.adjacency graph_from_adjacency_matrix
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
#' #subset to interactions between three species.
#' three <- c("Red-breasted Nuthatch","Downy Woodpecker","White-breasted Nuthatch")
#' small <- exInput[exInput$source %in% three & exInput$target %in% three,]
#'
#' #convert into igraph network
#' complexNetwork <- igraph::graph.data.frame(small)
#'
#' #plot to see what it looks like
#' plot(complexNetwork)
#'
#' #simplify with a 50% conservative threshold
#' simpleNetwork <- networkSimplifier(complexNetwork, 0.5, TRUE)
#'
#' #plot to see what it looks like
#' plot(simpleNetwork)

networkSimplifier <- function(sub.graph, cutoff, conservative)
{
	#convert the graph into an adjacency matrix
	adj <- igraph::get.adjacency(sub.graph)
	
	#find the total number of each interspecific interaction. there is no reason on my
	#personal computer to convert adj to a matrix below, but for some reason it fails on
	#the cluster when transposing it, and doing this makes that not happen
	totals <- adj + t(as.matrix(adj))
	
	#find the proportions of interactions each species wins. set all NaN elements to 0.
	#have to convert briefly to a matrix because can't seem to subset the S4 adjacency
	#matrix properly for nan
	props <- adj/totals
	props <- as.matrix(props)
	props[is.nan(props)] <- 0	
	
	#for each element [i,j], find element [j,i]. 
	for(i in 1:dim(adj)[1])
	{
		for(j in 1:dim(adj)[1])
		{
			#if the element is 0, leave it as is and skip to next element
			if(props[i,j] == 0)
			{
				adj[i,j] <- 0
				next
			}
			
			#if conservative is TRUE and the element is less than or equal to the cutoff,
			#set it to 0
			else if(conservative==TRUE & props[i,j] <= cutoff)
			{
				adj[i,j] <- 0
				next
			}

			#if conservative is TRUE and the element is greater than the
			#cutoff, set it to 1
			else if(conservative==TRUE & props[i,j] > cutoff)
			{
				adj[i,j] <- 1
				next
			}

			#if conservative is FALSE and the element is greater than or equal to the
			#cutoff, set it to 1
			else if(conservative==FALSE & props[i,j] >= cutoff)
			{
				adj[i,j] <- 1
				next
			}

			#this is the tricky part. if conservative is FALSE, and neither species is
			#above the cutoff, than we want to set both to 1s. if one species is above the
			#cutoff, than we want to set it to 1 and the other to 0. so if props[i,j] is
			#less than the cutoff AND greater than or equal to 1-cutoff, set to 1
			else if(conservative==FALSE & props[i,j] < cutoff & props[i,j] >= 1-cutoff)
			{
				adj[i,j] <- 1
				next
			}

			#if props[i,j] is less than the cutoff AND less than 1-cutoff, set to 0
			else if(conservative==FALSE & props[i,j] < cutoff & props[i,j] < 1-cutoff)
			{
				adj[i,j] <- 0
				next
			}

			else
			{
				stop("You did something wrong")
			}
		}
	}
	
	#convert to a graph and return
	toReturn <- igraph::graph_from_adjacency_matrix(adj)
	
	toReturn
}
