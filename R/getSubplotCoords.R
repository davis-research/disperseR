#' Get Points Within Min And Max Values By An Increment
#'
#' This function takes "minmax", which is a vector with a minimum and maximum
#' value, and "increment", which defaults to 25, and goes up by that increment
#' until it goes just over the maximum value. This is a helper function to draw
#' boxes around points.
#'
#' @param minmax A vector of two points, representing the minimum and maximum values.
#' @param increment The increment value.
#'
#' @return This function returns a vector of numbers encompassing the min and max values provided.
#'
#' @examples
#' getSubplotCoords()

getSubplotCoords <- function(minmax=c(0,100), increment=25){
  xcount <- minmax[1]+increment
  xmax <- minmax[2]
  response <- minmax[1]

  while(xcount < xmax){
    response <- c(response, xcount)
    xcount <- xcount+increment
  }
  response <- c(response, xcount) ## do last one, so it encompasses all points
  return(response)
}
