#' Derive standardized effect sizes per species/individual
#'
#' Use observed and randomized numbers of intransitive subnetworks per species/individual
#' to calculate standardized effect sizes.
#'
#' @param randomized Data frame of randomized transitivity results, from a call to spSig.
#' @param observed The results of a call to transitivityChecker and then transCruncher
#' with the same parameters as went into generating the randomized data frame.
#'
#' @details The user should separately derive randomized and observed numbers of
#' intransitive subnetworks each species is expected to be involved in, following the
#' format of the example below, then calculate the actual species' standardized effect
#' sizes with this function.
#'
#' @return Named numeric vector of species/individual-level standardized effect sizes.
#'
#' @export
#'
#' @importFrom stats median sd
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
#' #cause major issues with this example
#' toDelete <- list.files(tempdir(), pattern=".RDS")
#' file.remove(paste(tempdir(), toDelete, sep="/"))
#'
#' #subsample the data down and run a few iterations over it to illustrate function
#' exInput<-exInput[1:100,]
#' exInput[,1] <- as.factor(as.character(exInput[,1]))
#' exInput[,2] <- as.factor(as.character(exInput[,2]))
#'
#' #derive the randomized numbers of intransitive networks each species is expected
#' #to be involved in
#' random <- spSig(disp.input=exInput, network.size=3, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), iterations=3, cores=4)
#'
#' #derive the actual observed number of intransitive networks per species
#' transitivityChecker(disp.input=exInput, network.size=3, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#' actual <- transCruncher(3, TRUE, tempdir())
#'
#' #now calculate species' standardized effect scores. a bunch of these will be
#' #NA, because such a small number of randomized iterations
#' sppSES <- boilDown(randomized=random, observed=actual$intrans.results$`0.5`)
#' }

boilDown <- function(randomized, observed)
{
	means <- apply(randomized[,2:dim(randomized)[2]], 1, mean)
	sds <- apply(randomized[,2:dim(randomized)[2]], 1, sd)
	output <- (observed$intransitive - means)/sds
	names(output) <- observed$species
	output
}
