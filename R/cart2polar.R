#' Convert Cartesian To Polar
#'
#' This is a function that converts a set of cartesian coordinates to a set of
#' polar coordinates.
#'
#' @param x1 The x-coordinate of your origin.
#' @param y1 The y-coordinate of your origin.
#' @param x2 A number or vector of numbers of destination points.
#' @param y2 A number of vector of numbers of destination points.
#'
#' @return This function returns a data.frame with 4 columns. "x" is the
#'   distance between the xcoordinate and the origin, "y" is the distance
#'   between the y coordinate and the origin, theta is the angle from the
#'   original point, and r is the distance between the two points.
#'
#' @examples
#' cart2polar(1,1,c(-2,2,1),c(11,1,2))
#'
#' @export


cart2polar <- function(x1, y1, x2, y2){
  deltax <- x2-x1
  deltay <- y2-y1
  ## set to check for quads

  r <- sqrt(deltax^2 + deltay^2)
  theta <- atan(deltay/deltax)*(180/pi)

  quadrants <- data.frame(x=deltax,
                          y=deltay,
                          theta=theta,
                          r=r,
                          stringsAsFactors=FALSE)
  quadrants[which(quadrants$x >= 0 & quadrants$y >= 0), "theta"] <-
    quadrants[which(quadrants$x >= 0 & quadrants$y >= 0), "theta"]
  quadrants[which(quadrants$x < 0 & quadrants$y >= 0), "theta"] <-
    quadrants[which(quadrants$x < 0 & quadrants$y >= 0), "theta"]+180
  quadrants[which(quadrants$x < 0 & quadrants$y < 0), "theta"] <-
    quadrants[which(quadrants$x < 0 & quadrants$y < 0), "theta"]+180
  quadrants[which(quadrants$x >= 0 & quadrants$y < 0), "theta"] <-
    quadrants[which(quadrants$x >= 0 & quadrants$y < 0), "theta"]+360


  return(quadrants)
}
