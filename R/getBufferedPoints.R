#'Gets Points From A Rectangle With A Buffer
#'
#'This function finds the min and max values of x and y in a dataframe, then
#'picks n number of points within that buffer and returns them in vector form.
#'
#'@param x A vector of x values
#'@param y A vector of y values
#'@param buffer A value that indicates how much buffer there should be along
#'  each edge of the plot
#'@param n Optional. The number of points from the buffered area to sample from
#'@param systematic Optional, set to FALSE. If TRUE, this function will
#'  systematically sample based on the value in the parameter "by"
#'@param by Optional. This tells the function how to increment during systematic
#'  sampling.
#'
#'@return This function returns a data.frame of x and y values within the buffer
#'  zone designated in the function call.
#'
#'@examples
#'getRandomBufferedPoints(generatePlotMap()[,"x"],generatePlotMap()[,"y"],
#'buffer=10, n=5)
#'
#'@export

getBufferedPoints <- function(x, y, buffer, n=100, systematic=FALSE, by=1){
  ## Set the buffer zones
  minx <- min(x) + buffer
  maxx <- max(x) - buffer
  miny <- min(y) + buffer
  maxy <- max(y) - buffer

  ## sample x and y
  if(systematic==FALSE){
    samplex <- runif(n, min=minx, max=maxx)
    sampley <- runif(n, min=miny, max=maxy)


  } else{
    ##figure out how many iterations
    rangex <- maxx - minx
    rangey <- maxy - miny

    seqx <- seq(minx, maxx, by=by)
    seqy <- seq(miny, maxy, by=by)
    numy <- length(seqy)
    numx <- length(seqx)
    ## sort a replicated x change from min to max
    samplex <- sort(rep(seqx, numy))
    ## replicate y in normal order
    sampley <- rep(seqy, numx)
  }
  return(data.frame(x=samplex, y=sampley))
}
