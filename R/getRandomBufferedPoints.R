#'Gets Random Points From A Rectangle With A Buffer
#'
#'This function finds the min and max values of x and y in a dataframe, then
#'picks n number of points within that buffer and returns them in vector form.
#'
#'@param x A vector of x values
#'@param y A vector of y values
#'@param buffer A value that indicates how much buffer there should be along
#'  each edge of the plot
#'@param n The number of points from the buffered area to sample from
#'
#'@return This function returns a data.frame of x and y values within the buffer
#'  zone designated in the function call.
#'
#'@example
#'getRandomBufferedPoints(generatePlotMap()[,"x"],generatePlotMap()[,"y"],
#'buffer=10, n=5)
#'
#'@export

getRandomBufferedPoints <- function(x, y, buffer, n){
  ## Set the buffer zones
  minx <- min(x) + buffer
  maxx <- max(x) - buffer
  miny <- min(y) + buffer
  maxy <- max(y) - buffer

  ## sample x and y
  samplex <- runif(n, min=minx, max=maxx)
  sampley <- runif(n, min=miny, max=maxy)

  return(data.frame(x=samplex, y=sampley))

}
