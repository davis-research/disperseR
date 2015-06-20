#' Draw Box Around Point
#'
#' This function returns four points around a search radius of an original x,y set.
#'
#' @param x The x-coordinate of the center of your box.
#' @param y The y-coordinate of the center of your box.
#' @param buffer The distance, in coordinate space, of the searchradius
#'
#' @example
#' getBoxBoundaries(2, 5, 1)
#'
#' @export
 getBoxBoundaries <- function(x, y, buffer){
   leftx <- x - buffer
   rightx <- x + buffer
   topy <- y + buffer
   bottomy <- y - buffer
   return(c(leftx, rightx, bottomy, topy))
 }
