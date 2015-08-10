#' Count Adults In Radius
#'
#' This function searches in a radius around a series of x,y points and finds
#' any points within a radius, m, of those points.
#'
#' @param searchPoint An (x,y) vector of an "origin" point to search from.
#' @param forest An (x,y) data.frame of search points.
#' @param m The radius to search by.
#'
#' @return This function returns a number representing the total number of
#'   records found. This function operates on the dist() function in R, and does
#'   not search in a square, but rather, a circle.
#'
#' @examples
#'  countAdultsInRadius(c(1,2), data.frame(x=c(1:5), y=c(1:5)),6)
#'
#' @export

countAdultsInRadius <- function(searchPoint, forest, m){

  ## build search columns
  searching <- rbind(searchPoint, forest[, c("x", "y")])
  distmat <- as.matrix(dist(searching, diag=FALSE), rownames.force=TRUE)
  return(length(distmat[distmat[1,] < m & distmat[1,] > 0 ,1]))
}
