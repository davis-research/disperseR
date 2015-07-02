#' Convert Polar Coordinates To Cartesian Coordinates
#'
#' This function converts polar coordinates to cartesian coordinates. It needs
#' an origin (x,y), as well as an individual number or vector of numbers for
#' distance and bearing.
#'
#' @param x The origin x-coordinate. Numeric.
#' @param y The origin y-coordinate. Numeric.
#' @param dist A number or vector of numbers of distances from the origin.
#' @param bearing A number or vector of numbers representing the degrees away
#'   from the origin. This follows traditional polar coordinates, with 0 at
#'   "east", 90 at "north", etc.
#' @param as.deg Default set to true, if false, then the program assumes that
#'   "bearing" is entered in radians instead of degrees.
#' @param rnd The rounding value. Default is NA, for no rounding. Set to 0 for integers, 1 for 1 decimal point, etc.
#'
#' @return This function returns a list with "x" and "y" coordinates for the polar coordinates entered.
#' @examples
#' polar2cart(1, 1, 2, 90)
#' polar2cart(1,1,c(2,2,2),c(90,180,270) )
#'
#' @export
#'

polar2cart<-function(x,y,dist,bearing,as.deg=TRUE, rnd=NA){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing
  ## as.deg indicates if the bearing is in degrees (T) or radians (F)

  if(as.deg){
    ##if bearing is in degrees, convert to radians
    bearing=bearing*pi/180
  }

  newx<-x+dist*sin(bearing)  ##X
  newy<-y+dist*cos(bearing)  ##Y

  if(!is.na(rnd)){
    newx <- round(newx, rnd)
    newy <- round(newy, rnd)
  }
  return(data.frame("x"=newx,"y"=newy, stringsAsFactors=F))
}
