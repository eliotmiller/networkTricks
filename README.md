# networkTricks
## Code and sample data associated with our paper on dominance hierarchy of North American feeder birds

These functions are introduced in [our biorxiv paper](http://biorxiv.org/content/early/2017/01/30/104133). More documentation is included in the help files for the functions. Example data is included with the R package, and the full dataset can be downloaded directly from [Project Feederwatch](http://feederwatch.org). A few example functions are highlighted here.

```r
library(devtools)
install_github("networkTricks/eliotmiller")
library(networkTricks)

# calculate our slightly modified form of Bradley-Terry model dominance coefficients.
# first load 1500 lines of sample interaction data from the larger FeederWatch dataset.
data(exInput)

# run the Bradley Terry function. takes a few seconds to run, can run in parallel.
results <- myBTM(edge.list=exInput, cores=2)

#the names of these scores have weird periods in them. use agrep to solve
species <- unique(c(as.character(exInput$source), as.character(exInput$target)))

names(results) <- sub("..", "", x=names(results))

for(i in 1:length(results))
{
   names(results)[i] <- species[agrep(names(results)[i], species, max.distance=0.1)]
}

# here are the species' level dominance coefficients
results

# here's an example of how to threshold a network, e.g. interactions between three species.
three <- c("Red-breasted Nuthatch","Downy Woodpecker","White-breasted Nuthatch")
small <- exInput[exInput$source %in% three & exInput$target %in% three,]

# convert into igraph network
complexNetwork <- igraph::graph.data.frame(small)

# plot to see what it looks like
plot(complexNetwork)

# simplify with a 50% conservative threshold
simpleNetwork <- networkSimplifier(complexNetwork, 0.5, TRUE)

# plot to see what it looks like
plot(simpleNetwork)

# here is an example of how to plot an attribute-ordered network, sensu [Hobson et al. (2015)](https://academic.oup.com/cz/article/61/1/55/1792913/The-effect-of-social-context-and-social-scale-on?searchresult=1). 
# prep the object for plotting
prepped <- prepAON(results, exInput)

plotAON(prepped$prepped, dom.color="#00009985", sub.color="#CC000095", xlim=c(-1,-0.8),
	vertex.size=0.1, vertex.color=NA, vertex.frame.color=NA, vertex.label.color="black",
	vertex.label.cex=0.28, vertex.color="white", vertex.label.family="sans",
	edge.width.scaler=10, edge.arrow.size=0, edge.curvature=TRUE)

# easy to modify things such as edge width, color, and how much they curve
plotAON(prepped$prepped, dom.color="#5ab4ac", sub.color="#d8b365", xlim=c(-1,-0.8),
	vertex.size=0.1, vertex.color=NA, vertex.frame.color=NA, vertex.label.color="black",
	vertex.label.cex=0.28, vertex.color="white", vertex.label.family="sans",
	edge.width.scaler=6, edge.arrow.size=0, edge.curvature=2)
