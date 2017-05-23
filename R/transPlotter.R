#' Plot function for transitivity results
#'
#' Plot the summarized results from multiple calls to transitivityChecker.
#'
#' @param results.list A list of results from multiple calls to transitivityChecker.
#'
#' @details Uses a Brewer color palette to plot the results of multiple calls to
#' transitivityChecker(). Each unique subnetwork size gets its own color.
#'
#' @return An R plot.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics lines plot
#'
#' @export
#'
#' @references Miller, E. T., D. N. Bonter, C. Eldermire, B. G. Freeman, E. I. Greig,
#' L. J. Harmon, C. Lisle, and W. M. Hochachka. 2017. Fighting over food unites the
#' birds of North America in a continental dominance hierarchy.
#' biorxiv https://doi.org/10.1101/104133
#'
#' @examples
#' #load in the example data
#' \dontrun{
#' data(exInput)
#'
#' #look at transitivity of dyads. this whole thing will take a few minutes
#' #to run. be patient.
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.6,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.7,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.8,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.9,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=1.0,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' #now look at transitivity of a couple triad thresholds
#' transitivityChecker(disp.input=exInput, network.size=3, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' transitivityChecker(disp.input=exInput, network.size=3, cutoff=1.0,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' twoSpp <- transCruncher(2, TRUE, tempdir())
#' threeSpp <- transCruncher(3, TRUE, tempdir())
#'
#' #bind together just the count results for plotting
#' toPlot <- list(twoSpp$counts, threeSpp$counts)
#'
#' transPlotter(toPlot)
#' }

transPlotter <- function(results.list)
{
	cols <- RColorBrewer::brewer.pal(n=length(results.list), "Set2")
	
	#quickly subset results.list to only those instances that do not have NA, to better
	#get y lims
	
	temp <- Reduce(rbind, results.list)
	temp <- temp[temp$threshold!="NA",]
	
	yMin <- min(temp$prop.false)
	yMax <- max(temp$prop.false)

	plot(results.list[[1]]$prop.false~results.list[[1]]$threshold, type="n",
		ylim=c(yMin, yMax), xlab="Threshold", ylab="Proportion intransitive")

	lines(results.list[[1]]$prop.false~results.list[[1]]$threshold,
			col=cols[1], lwd=5)

	for(i in 2:length(results.list))
	{
		lines(results.list[[i]]$prop.false~results.list[[i]]$threshold,
			col=cols[i], lwd=5)
	}
}
