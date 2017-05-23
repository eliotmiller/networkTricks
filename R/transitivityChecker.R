#' Run the DAG method over a social network
#'
#' Calculate transitivity of a network using the directed acyclic graph method.
#'
#' @param disp.input An edge-list-like data frame, formatted like the exInput data.
#' @param network.size The subnetwork size (dyad, triad, quartet, etc.) at which to
#' assess transitivity.
#' @param cutoff The proportion of interactions a species needs to have won in order to
#' be considered the winner.
#' @param conservative TRUE or FALSE. Whether a tied interaction is set to a
#' bidirectional edge or the edges are removed entirely.
#' @param write.wd The path to the working directory where results will be written.
#' @param cores The number of cores to employ for parallel processing. Set to 'seq' to
#' run sequentially (i.e. not in parallel).
#'
#' @details Takes a larger dominance hierarchy/social network, and splits it into all
#' possible subnetworks of the specified size. Simplifies each of those networks
#' according to the arguments provided, then assesses subnetwork transitivity. Writes
#' all results to write.wd. These results can be summarized in subsequent steps with
#' transCruncher().
#'
#' @return Nothing to the workspace. Writes two RDS files to write.wd.
#'
#' @export
#'
#' @importFrom inline cfunction
#' @importFrom iterators iter
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom igraph is.dag
#' @importFrom utils combn read.table write.table
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
#' #look at transitivity of dyads
#' transitivityChecker(disp.input=exInput, network.size=2, cutoff=0.5,
#'   conservative=TRUE, write.wd=tempdir(), cores=4)
#'
#' #here are all the possible subnetworks
#' allPoss <- readRDS(paste(tempdir(), "combn_results_2species_cutoff0.5.RDS",
#'   sep="/"))
#'
#' #and here are transitivity results
#' transResults <- readRDS(paste(tempdir(),
#'    "trans_results_2species_cutoff0.5consTRUE.RDS", sep="/"))
#'
#' #here are all transitive networks we had sufficient information to assess
#' allPoss[,which(transResults==TRUE)]
#' }

transitivityChecker <- function(disp.input, network.size, cutoff, conservative,
	write.wd, cores)
{
	#force to character
	disp.input <- data.frame(apply(disp.input, 2, as.character),
		stringsAsFactors=FALSE)
	
	#define the species involved
	species <- unique(c(disp.input$source, disp.input$target))

	if(missing(conservative))
	{
		stop("conservative must be set to TRUE or FALSE")
	}

	#to save time and memory costs, check if a relevant combn file already exists.
	#this is dangerous in that the combn could accidentally be from a previous run
	filePattern <- paste(network.size, "species", sep="")
	allFiles <- list.files(write.wd, pattern=filePattern)

	if(length(allFiles) > 0)
	{
		#there could be many relevant files, but randomly read the first
		temp <- readRDS(paste(write.wd, allFiles[[1]], sep="/"))
	}

	#if no relevant file, create it from scratch
	else
	{
		#create an array expressing all unique permutations of a given size
		temp <- combn(species, network.size)
	}

	#save out the combn results
	saveName <- paste("combn_results_", network.size, "species_",
		"cutoff", cutoff,  ".RDS", sep="")
	saveRDS(temp, file=paste(write.wd, saveName, sep="/"))

	#if cores != "seq", start parallel processing and save out an RDS file of the
	#results.
	if(cores != "seq")
	{
		#some code from below URL to avoid zombie processes
		#http://stackoverflow.com/questions/25388139/
		#r-parallel-computing-and-zombie-processes
		includes <- '#include <sys/wait.h>'
		code <- 'int wstat; while (waitpid(-1, &wstat, WNOHANG) > 0) {};'
		wait <- inline::cfunction(body=code, includes=includes, convention='.C')

		#split the combn results up into the iterators format so that it does not
		# take asmuch memory
		prepped <- iterators::iter(temp, by="column")

		doParallel::registerDoParallel(cores)
	
		#run through each element of the iterators object here
		singleResult <- foreach(i = prepped, .combine=c) %dopar%
		{
			#subset the interactions to just those between the species in the subnetwork
			subInteractions <- disp.input[disp.input$source %in% i &
				disp.input$target %in% i, c("source", "target")]

			#if there are no interactions between that set of species, return NA
			if(dim(subInteractions)[1] < 1)
			{
				holdMe <- NA
			}
			#otherwise create a network graph and see if DAG
			else
			{
				tempGraph <- igraph::graph.data.frame(subInteractions)
				
				#before testing whether dag, unless cutoff is NA, simplify network
				if(!is.na(cutoff))
				{
					tempGraph <- networkSimplifier(tempGraph, cutoff=cutoff,
						conservative=conservative)
				}
				
				holdMe <- igraph::is.dag(tempGraph)
			}
			holdMe
		}
	
		#call the wait function you defined above
		wait()	
	
		#save out the singleResult
		saveName <- paste("trans_results_", network.size, "species_",
			"cutoff", cutoff, "cons", conservative,  ".RDS", sep="")
		saveRDS(singleResult, file=paste(write.wd, saveName, sep="/"))
	}

	else if(cores=="seq")
	{
		#prep the save names
		saveName <- paste("trans_results_", network.size, "species_",
			"cutoff", cutoff, "cons", conservative,  ".RDS", sep="")
		tempName <- sub(".RDS", ".txt", saveName)

		#run sequentially through the combn results
		for(i in 1:dim(temp)[2])
		{
			#subset the interactions to just those between the species in the subnetwork
			subInteractions <- disp.input[disp.input$source %in% temp[,i] &
				disp.input$target %in% temp[,i], c("source", "target")]

			#if there are no interactions between that set of species, return NA
			if(dim(subInteractions)[1] < 1)
			{
				holdMe <- NA
			}
			#otherwise create a network graph and see if DAG
			else
			{
				tempGraph <- graph.data.frame(subInteractions)
				
				#before testing whether dag, unless cutoff is NA, simplify network
				if(!is.na(cutoff))
				{
					tempGraph <- networkSimplifier(tempGraph, cutoff=cutoff,
						conservative=conservative)
				}
				
				holdMe <- is.dag(tempGraph)
			}
			
			#if i is 1, create a text file you will write the results into as they come
			#off, so you can pay attention to status of the run.
			if(i==1)
			{
				write.table(holdMe, file=paste(write.wd, tempName, sep="/"),
					row.names=FALSE, col.names=FALSE, append=FALSE)
			}
			else
			{
				write.table(holdMe, file=paste(write.wd, tempName, sep="/"),
					row.names=FALSE, col.names=FALSE, append=TRUE)
			}
		}

		#read in the table you were saving to and then save it out in an RDS format that
		#matches that of the way it would be if you'd run it in parallel
		toResave <- read.table(file=paste(write.wd, tempName, sep="/"))
		saveRDS(toResave[,1], file=paste(write.wd, saveName, sep="/"))
	}
	else
	{
		stop("cores must either be set to 'seq' or a value")
	}
}
