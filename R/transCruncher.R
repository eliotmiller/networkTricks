#' Summarize results of transitivityChecker
#'
#' Use previously run results of transitivityChecker to summarize transitivity of
#' networks, possibly over multiple runs of transitivityChecker.
#'
#' @param network.size The size of the subnetworks over which the user would like to
#' summarize network transitivity.
#' @param conservative Whether to summarize results over calls to transitivityChecker
#' that were thresholded according to a conservative (TRUE) or a liberal (FALSE) rule.
#' @param read.wd The path where the results are saved.
#'
#' @details This function is used to summarize the transitivity of a larger network as
#' assessed at a given subnetwork size and tie-handling method (conservative or not).
#' For example, a series of analyses at the same subnetwork size and tie-handling method
#' but varying threshold might be run, then this function would summarize the results.
#'
#' @return A list of result objects. First, a list, where each element in that list is
#' a data frame of results at a given threshold. That data frame provides the number of
#' total subnetworks that each species was involved in, how many of those were
#' intransitive, and the proportion of intransitivie subnetworks each was involved in.
#' Second, a data frame summarizing the total number of transitive, intransitive, and
#' NA subnetworks in the larger network.
#'
#' @export
#'
#' @importFrom inline cfunction
#' @importFrom iterators iter
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom igraph is.dag
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
#' #look at transitivity of dyads
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' #assess transitivity of these dyads
#' transCruncher(2, TRUE, tempdir())
#' }

transCruncher <- function(network.size, conservative, read.wd)
{
	#identify all trans files in read.wd that contain phrases about whatever you set
	#network.size and conservative to.
	
	conStatus <- paste("cons", conservative, sep="")
	tempFiles <- list.files(read.wd)[grep(conStatus, list.files(read.wd))]

	spNo <- paste(network.size, "species", sep="")
	transFiles <- tempFiles[grep(spNo, tempFiles)]
	
	#load these files in
	trans <- lapply(transFiles, function(x) readRDS(paste(read.wd, x, sep="/")))

	#load the combn file in. you only need one of these. note the 1 suffix here
	tempFiles <- list.files(read.wd)[grep("combn", list.files(read.wd))]
	combnFile <- tempFiles[grep(spNo, tempFiles)][1]
	combns <- readRDS(paste(read.wd, combnFile, sep="/"))
	
	#split out the threshold values for each of these loaded files and apply as names.
	#this is really hacky and prone to name changes in how things are saved
	tempNames <- strsplit(transFiles, "cutoff")
	tempNames <- unlist(lapply(tempNames, "[", 2))
	tempNames <- strsplit(tempNames, "cons")
	transNames <- lapply(tempNames, "[", 1)
	names(trans) <- transNames

	#for each threshold level trans result, figure out how many subnetworks each species
	#was involved in, and figure out how many of those were intransitive. set up blank
	#results files here
	
	intransResults <- list()
	
	for(i in 1:length(trans))
	{
		#this will determine how many subnetworks each species was involved in. first
		#subset to elements from combns where we had information
		temp <- combns[,which(!is.na(trans[[i]]))]
		
		#then string to a vector 
		allNetworks <- as.vector(temp)
		
		#summarize occurrence of each species
		sppTotalCounts <- plyr::count(allNetworks)
		
		#rename columns
		names(sppTotalCounts) <- c("species", "total")
		
		#now subset to intransitive networks
		temp <- combns[,which(trans[[i]]==FALSE)]
		intransNetworks <- as.vector(temp)
		
		#summarize intransitive occurrences of each species
		sppIntransCounts <- plyr::count(intransNetworks)

		#rename columns and merge, keeping all entries
		names(sppIntransCounts) <- c("species", "intransitive")
		
		singleResult <- merge(sppTotalCounts, sppIntransCounts, all.x=TRUE, 
			all.y=TRUE)
		
		#replace the NA intransitive with zeros, then calculate proportion of intrans
		singleResult$intransitive[is.na(singleResult$intransitive)] <- 0
		singleResult$prop.intransitive <- singleResult$intransitive/singleResult$total

		intransResults[[i]] <- singleResult
	}

	#give names to intransResults
	names(intransResults) <- transNames
	
	#run plyr count over the trans results
	counts <- lapply(trans, plyr::count)
	
	#convert to character
	lapply(counts, function(y) as.character(y$x))
	
	#add a FALSE row with 0 if that was missing
	toAdd <- data.frame(x="FALSE", freq=0, stringsAsFactors=FALSE)
	
	for(i in 1:length(counts))
	{
		if(dim(counts[[i]])[1] < 3)
		{
			#note we add toAdd first, so that FALSE row is on top, otherwise will cbind
			#without returning errors but will mess it up
			counts[[i]] <- rbind(toAdd, counts[[i]])
		}
	}

	#now cbind together, get rid of the excess TRUE/FALSE columns
	#and bind in the threshold values
	counts <- Reduce(cbind, counts)	
	counts[,grep("x", names(counts))] <- NULL
	counts <- t(counts)
	row.names(counts) <- NULL
	counts <- data.frame(counts)
	names(counts) <- c("FALSE", "TRUE", "NA")
	counts$threshold <- names(trans)
	
	#calculate the proportion of intransitive networks
	counts$prop.false <- counts$'FALSE'/apply(counts[,1:2], 1, sum)
	
	#sort counts into order by threshold
	counts <- counts[order(counts$threshold),]
	
	#bind both into a big results files
	results <- list("intrans.results" = intransResults, "counts"=counts)
	results
}
