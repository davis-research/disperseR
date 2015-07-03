#' Find Rotation Factor For Plot
#'
#' Given (x,y) of the left-most and bottom-most points in a dataframe, find the
#' angle required to rotate the plot to be straight. This function will return
#' the degrees of rotation required to turn a plot straight. Specifically, it
#' converts these two points into polar coordinates, then subtracts the angle of
#' their interaction from 360 degrees. Used primarily within other functions
#' like rotatePlot.
#'
#' @param xleft The x value of the leftmost point.
#' @param yleft The y value of the leftmost point.
#' @param xbot The x value of the bottommost point.
#' @param ybot The y value of the bottommost point.
#'
#' @return This function returns a single numeric value representing the
#'   conversion factor needed to straighten a plot based on its leftmost and
#'   bottommost points. In essence, if the leftmost point is above the
#'   bottommost point in space, it will rotate counter clockwise until the y's
#'   of each point are equal. If they are already equal, no conversion will
#'   occur.
#' @examples
#' findRotation(0,0,1,-1)
#' findRotation(0,2,0,0)
#'
#'@export

findRotation <- function(xleft, yleft, xbot, ybot){

  ## make sure that "leftmost" and "bottommost" are actually accurate.
  ##
  if(xleft > xbot | ybot >= yleft){
    stop("Sorry, you have not given the leftmost or bottommost points. Either xleft > xbot or ybot >= yleft.")
  }

  conversion <- 360-cart2polar(xleft, yleft, xbot, ybot)$theta

  return(conversion)
}
