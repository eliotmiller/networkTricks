#' Prep data for AON plot
#'
#' Prepare data for an attribute-ordered network plot.
#'
#' @param prepped The prepped element from the prepAON object. See example.
#' @param dom.color Color to draw edges connecting dominant to subordinate nodes.
#' Can take various forms, e.g. hexadecimal code or the name of the color.
#' @param sub.color Color to draw edges connecting subordinate to dominant nodes.
#' @param edge.width.scaler Varies the edge width connecting nodes. Functions as a
#' scaler, not an absolute. In other words, rarely observed interactions should be
#' plotted no matter what, but frequently observed interactions will be plotted less
#' thickly. Larger values of this scaler more heavily 'thin' frequently observed
#' interactions.
#' @param edge.curvature Set to TRUE to use default degree of curvature in lines.
#' Set to a numeric value to more carefully fine tune the degree of curvature. See
#' example. Can set to FALSE, but resulting plot unlikely to be useful.
#' @param ... Other arguments can be passed to plot.igraph here, which should
#' theoretically encompass all options available to igraph.plotting.
#'
#' @details User should format edge.list so it looks like the example data (exInput).
#'
#' @return A list object, prepped for use in plotting an attribute-ordered network.
#'
#' @export
#'
#' @importFrom igraph E V plot.igraph
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
#'
#' plotAON(prepped$prepped, dom.color="#00009985", sub.color="#CC000095", xlim=c(-1,-0.8),
#'	vertex.size=0.1, vertex.color=NA, vertex.frame.color=NA, vertex.label.color="black",
#'	vertex.label.cex=0.28, vertex.color="white", vertex.label.family="sans",
#'	edge.width.scaler=10, edge.arrow.size=0, edge.curvature=TRUE)
#'
#' #easy to modify things such as edge width, color, and how much they curve
#' plotAON(prepped$prepped, dom.color="#5ab4ac", sub.color="#d8b365", xlim=c(-1,-0.8),
#'	vertex.size=0.1, vertex.color=NA, vertex.frame.color=NA, vertex.label.color="black",
#'	vertex.label.cex=0.28, vertex.color="white", vertex.label.family="sans",
#'	edge.width.scaler=6, edge.arrow.size=0, edge.curvature=2)

plotAON <- function(prepped, dom.color, sub.color, edge.width.scaler,
	edge.curvature, ...)
{
	#adding this to avoid throwing notes in devtools check
	rank.diff <- NA

	#set edge.curvature to TRUE if missing
	if(missing(edge.curvature))
	{
		edge.curvature <- TRUE
	}
	else
	{
		edge.curvature <- edge.curvature
	}
	
	# Add conditional color, based on direction of aggression
	#"UP" ties as slightly transparent red, "DOWN" ties as slightly transparent blue
	igraph::E(prepped)[rank.diff<0]$color <- sub.color
	igraph::E(prepped)[rank.diff>0]$color <- dom.color

	# Set custom attribute-ordered network layout
	# set layout using x/y coords from attributes
	x <- igraph::V(prepped)$x
	y <- igraph::V(prepped)$rank

	#attribute-ordered layout of nodes
	attord <- cbind(x, y)
	
	igraph::plot.igraph(prepped,
		layout=attord,
		edge.width=E(prepped)$wins/edge.width.scaler,
		edge.curved=edge.curvature,
		edge.loop.angle=1,
		edge.color=E(prepped)$color,
		...)
}
