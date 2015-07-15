#' Find Adult Trees Around A Set Of Points
#'
#' This function finds the individual adult trees within "m" distance of each
#' seedling subplot in seedPlots. seedPlots and adults are both data.frames, and
#' m is the search distance.
#'
#' @param seedPlots A data.frame containing "x", "y" and "n", the geographic
#'   location of seedling plots and the number of seedlings contained within
#'   those plots.
#' @param adults A data.frame containing "x", "y" and "dbh" of adult trees in
#'   the plot, along with a unique identifier "treeid".
#' @param m The search radius, from the center of the seedling Plot. This does
#'   not take into account the previous "m" for seedling subplots that were
#'   generated.
#'
#' @return This function returns a data.frame with "treeid", "ri", "m" and "dbh". "ri" is the number of seedlings in the subplot that was "m" distance from the tree labeled "treeid" with a "dbh".
#'
#' @examples
#' #' ## make a sample seed data.frame by subsetting the included
#' ## expandedTrees data.frame..
#' sampleseeds <- expandedTrees[expandedTrees$plot=="trinity" &
#'                              expandedTrees$measyear==2001 &
#'                              expandedTrees$stage=="seedling" &
#'                              expandedTrees$species=="ABCO",]
#' ## get subplots
#' seedlingPlots <- findSeedPlots(sampleseeds, 1)
#' ## get adult trees
#' sampleadults <- expandedTrees[expandedTrees$plot=="trinity" &
#'                              expandedTrees$measyear==2001 &
#'                              expandedTrees$stage=="tree" &
#'                              expandedTrees$species=="ABCO",]
#' ## run function
#' adults <- findAdultTrees(seedlingPlots, sampleadults, 20)
#'
#' ## look at data
#' head(adults)
#' nrow(adults)
#' @export


findAdultTrees <- function(seedPlots, adults, m){
  ## initialize response table
  responsetable <- data.frame(treeid=NA, ri=NA, m=NA, dbh=NA, stringsAsFactors=FALSE)

  ##convert row numbers to treeids. We do this because the matrix uses row
  ##numbers, not treeids, to associate with values. Also, we can get away with
  ##it because this function requires unique treeids.
  if(length(unique(adults$treeid)) != nrow(adults)){
    stop("Error: You have repeated elements in your treeid column. Treeid must be a unique identifier. Check to make sure that you're entering your data.frames into the function correctly.")
  }


  rownames(adults) <- adults$treeid

  ## for each seedling plot...
  for(i in 1:nrow(seedPlots)){
    distmat <- as.matrix(
      dist(rbind(seedPlots[i,c("x", "y")], adults[,c("x", "y")]),
           diag=FALSE), rownames.force=TRUE)
    close <- distmat[distmat[,1]<=m & distmat[,1]>0,1]
    if(length(close) != 0){
    treeids <- names(close)
    dbhs <- adults[charmatch(treeids, adults$treeid),"dbh"]
    if(length(dbhs)!=0){
    newdata <- data.frame(treeid=treeids, ri=seedPlots[i, "n"], m=close, dbh=dbhs, stringsAsFactors=FALSE)

    responsetable <- rbind(responsetable, newdata)
    }
    }
  }
  return(cleanResponse(responsetable,1))

}
