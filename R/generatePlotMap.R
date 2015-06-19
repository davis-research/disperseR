#'Generate Plot Map
#'
#'This generates a random plot map for testing.
#'
#'@param species The number of species to generate within the plot
#'@param plotlength The length of the plot (x axis)
#'@param plotwidth The width of the plot (y axis)
#'@param maxsize A number or vector of the max DBH size of the trees
#'@param propSeeds The number (or vector for each species) of the proportion of
#'  trees generated that should be seedlings
#'@param maxnum A number of vector of number of trees per species
#'
#'@example
#'generatePlotMap(species=3, plotlength=100, plotwidth=100,
#'maxsize=100, propSeeds=0.5, maxnum=20)
#'
#'@export

generatePlotMap <- function(species=3, plotlength=100, plotwidth=100, maxsize=100, propSeeds=0.5, maxnum=20){
  ## figure out how many trees to create. If there's only one number in maxnum,
  ## multiple it by species to get total rows needed
    if(length(maxnum)==1){
      totalrows <- maxnum * species
      maxnum <- rep(maxnum, species)
    } else{
      totalrows <- sum(maxnum)
    }

  ## figure out the proportion of seeds needed
  if(length(propSeeds)==1){
    propSeeds <- rep(propSeeds, species)
  }

  totseeds <- round(maxnum*propSeeds)
  totadults <- maxnum-totseeds
  ## generate species column
    speciesvec <- NULL
    dbhvec <- NULL
    for(i in 1:species){
      speciesvec <- c(speciesvec, rep(i, maxnum[i]))
      dbhvec <- c(rep(NA, totseeds[i]), runif(totadults[i], min=0, max=maxsize))
    }

  ## generate x's and y's
    xcoord <- runif(totalrows, min=0, max=plotlength)
    ycoord <- runif(totalrows, min=0, max=plotwidth)

  ## generate DBH's

  treemap <- data.frame(species=speciesvec, x=xcoord, y=ycoord, dbh=dbhvec)
  return(treemap)
}
